use std::thread;
use std::slice;
use std::str;
use std::time::{Duration, Instant};

use emacs::{defun, Result, Env, IntoLisp};

use objc::{
    runtime::{Class, Object, YES},
    rc::autoreleasepool,
};
use cocoa::appkit::NSPasteboard;
use cocoa::base::{nil, id};
use cocoa::foundation::{NSString, NSArray, NSUInteger};

#[link(name = "ScriptingBridge", kind = "framework")]
extern "C" {}

/// Hacky wrapper type that allows constructing objects with lazy_static. The safety contract is
/// that the inner value must be used in an immutable manner.
struct Immutable {
    id: id,
}

unsafe impl Sync for Immutable {}

impl Immutable {
    unsafe fn new(id: id) -> Self {
        Self { id }
    }
}

fn nsstring(s: &str) -> id {
    unsafe { NSString::alloc(nil).init_str(s) }
}

lazy_static::lazy_static! {
    static ref FRONTMOST: Immutable = unsafe {
        let fmt: id = nsstring("frontmost = YES");
        let pred: id = msg_send![class!(NSPredicate), predicateWithFormat:fmt];
        Immutable::new(pred)
    };
    static ref SYSTEM_EVENTS: Immutable = unsafe {
        let s = nsstring("com.apple.systemevents");
        Immutable::new(msg_send![class!(SBApplication), applicationWithBundleIdentifier:s])
    };
}

// TODO: Extract this pattern into a macro.
pub fn init(_: &Env) -> Result<()> {
    // We need to initialize early, not on first access, since an autorelease pool may be active
    // at that point.
    lazy_static::initialize(&FRONTMOST);
    lazy_static::initialize(&SYSTEM_EVENTS);
    Ok(())
}


/// Get and print an objects description.
pub unsafe fn describe(obj: id) {
    let description: *mut Object = msg_send![obj, description];
    if let Some(desc_str) = to_str(description) {
        println!("    : {}", desc_str);
    } else {
        println!("    : nil");
    }
}

/// Convert an NSString to a String.
fn to_str<'a, T: NSString + Copy>(nsstring_obj: T) -> Option<&'a str> {
    let bytes = unsafe {
        let length = nsstring_obj.len();
        let utf8_str = nsstring_obj.UTF8String() as *const u8;
        slice::from_raw_parts(utf8_str, length)
    };
    str::from_utf8(bytes).ok()
}

// TODO: This should be in the `emacs` crate.
/// This should be used instead of thread::sleep, so that the GIL is released for the duration of
/// the sleep, enabling other Lisp threads to run.
fn sleep(env: &Env, seconds: f64) -> Result<()> {
    env.call("sleep-for", &[seconds.into_lisp(env)?])?;
    Ok(())
}

macro_rules! first {
    ($obj:expr, $name:ident) => {
        {
            let list: id = msg_send![$obj, $name];
            msg_send![list, firstObject]
        }
    };
}

unsafe fn list_processes() -> id {
    msg_send![SYSTEM_EVENTS.id, applicationProcesses]
}

unsafe fn get(list: id, pred: id) -> id {
    let filtered: id = msg_send![list, filteredArrayUsingPredicate: pred];
    msg_send![filtered, firstObject]
}

unsafe fn get_process(pred: id) -> id {
    get(list_processes(), pred)
}

#[defun]
fn _frontmost_bundle_identifier() -> Result<String> {
    autoreleasepool(|| unsafe {
        let x = msg_send![class!(NSThread), isMultiThreaded];
        describe(x);
        let frontmost = get_process(FRONTMOST.id);
        let bundle: id = msg_send![frontmost, bundleIdentifier];
        Ok(to_str(bundle).expect("No frontmost application process was found").to_owned())
    })
}

#[defun]
fn _copy_text(bundle_identifier: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        let fmt: id = nsstring("bundleIdentifier == %@");
        let args = NSArray::arrayWithObject(nil, nsstring(&bundle_identifier));
        let pred = msg_send![class!(NSPredicate), predicateWithFormat:fmt argumentArray:args];
        let process = get_process(pred);
        let menu_bar: id = first![process, menuBars];
        let menu_bar_item: id = first![menu_bar, menuBarItems];
        let windows: id = msg_send![process, windows];
        let x = msg_send![windows, get];
        describe(menu_bar);
        describe(x);
    });
    Ok(())
}

/// Return the current "changeCount" of the clipboard.
#[defun]
fn _change_count() -> Result<i64> {
    Ok(unsafe { NSPasteboard::generalPasteboard(nil).changeCount() })
}

/// Repeatedly poll the clipboard for new items, returning the "changeCount" of the clipboard. If no
/// item is copied to the clipboard after the given number of milliseconds, return nil.
///
/// This should be run in a background thread.
#[defun]
fn _wait_for_clipboard(env: &Env, timeout: i64, count: Option<i64>) -> Result<Option<i64>> {
    let pb = unsafe { NSPasteboard::generalPasteboard(nil) };
    let start = Instant::now();
    let count = count.unwrap_or_else(|| unsafe { pb.changeCount() });
    let timeout = Duration::from_millis(timeout as u64);
    loop {
        let new_count = unsafe { pb.changeCount() };
        if new_count != count {
            return Ok(Some(new_count));
        }
        if start.elapsed() > timeout {
            return Ok(None);
        }
        sleep(env, 0.02)?;
    }
}
