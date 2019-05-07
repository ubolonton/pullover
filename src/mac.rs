use std::slice;
use std::str;
use std::time::{Duration, Instant};

use emacs::{defun, Result, Env, IntoLisp};

use objc::{
    runtime::Object,
    rc::autoreleasepool,
};
use cocoa::appkit::NSPasteboard;
use cocoa::base::{nil, id, YES};
use cocoa::foundation::{NSString, NSArray, NSDictionary};

#[link(name = "ScriptingBridge", kind = "framework")]
extern "C" {}

fn nsstring(s: &str) -> id {
    unsafe { NSString::alloc(nil).init_str(s) }
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

unsafe fn list_processes() -> id {
    let s = nsstring("com.apple.systemevents");
    let system_events: id = msg_send![class!(SBApplication), applicationWithBundleIdentifier: s];
    msg_send![system_events, processes]
}

unsafe fn filter(list: id, pred: id) -> id {
    msg_send![list, filteredArrayUsingPredicate: pred]
}

unsafe fn first(list: id) -> id {
    msg_send![list, firstObject]
}

unsafe fn get(list: id, pred: id) -> id {
    first(filter(list, pred))
}

unsafe fn get_process(pred: id) -> id {
    get(list_processes(), pred)
}

unsafe fn name_is(name: &str) -> id {
    let fmt: id = nsstring("name = %@");
    let args = NSArray::arrayWithObject(nil, nsstring(name));
    msg_send![class!(NSPredicate), predicateWithFormat:fmt argumentArray:args]
}

unsafe fn bundle_id_is(b_id: &str) -> id {
    let fmt: id = nsstring("bundleIdentifier == %@");
    let args = NSArray::arrayWithObject(nil, nsstring(b_id));
    msg_send![class!(NSPredicate), predicateWithFormat:fmt argumentArray:args]
}

unsafe fn click(process: id, item: id) -> id {
    msg_send![process, click:item at:0]
}

macro_rules! f {
    ($obj:expr, $list_name:ident) => {{
        let list: id = msg_send![$obj, $list_name];
        first(list)
    }};
    ($obj:expr, $list_name:ident, name = $name:expr) => {{
        let list: id = msg_send![$obj, $list_name];
        first(filter(list, name_is($name)))
    }};
}

#[defun]
fn _frontmost_bundle_id() -> Result<String> {
    autoreleasepool(|| unsafe {
        let fmt: id = nsstring("frontmost = YES");
        let pred: id = msg_send![class!(NSPredicate), predicateWithFormat: fmt];
        let frontmost = get_process(pred);
        let bundle: id = msg_send![frontmost, bundleIdentifier];
        Ok(to_str(bundle).expect("No frontmost application process was found").to_owned())
    })
}

#[defun]
fn _copy_text(bundle_id: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        let pred = bundle_id_is(&bundle_id);
        let process = get_process(pred);
        let menu_bar: id = f![process, menuBars];
        let edit: id = f![menu_bar, menuBarItems, name = "Edit"];
        let menu: id = f![edit, menus];
        let select_all: id = f![menu, menuItems, name = "Select All"];
        let copy: id = f![menu, menuItems, name = "Copy"];
        click(process, select_all);
        click(process, copy);
    });
    Ok(())
}

#[defun]
fn _paste_text(bundle_id: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        let pred = bundle_id_is(&bundle_id);
        let process = get_process(pred);
        msg_send![process, setFrontmost: YES];
        let menu_bar: id = f![process, menuBars];
        let edit: id = f![menu_bar, menuBarItems, name = "Edit"];
        let menu: id = f![edit, menus];
        let select_all: id = f![menu, menuItems, name = "Select All"];
        let paste: id = f![menu, menuItems, name = "Paste"];
        click(process, select_all);
        click(process, paste);
    });
    Ok(())
}

#[defun]
fn _activate(bundle_id: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        let pred = bundle_id_is(&bundle_id);
        // TODO: Get the app instead of getting its process through SystemEvents.
        let process = get_process(pred);
        msg_send![process, setFrontmost: YES];
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
