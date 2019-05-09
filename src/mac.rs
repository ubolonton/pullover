use emacs::{defun, Result};

use std::{slice, str, process};

use objc::rc::autoreleasepool;
use cocoa::{
    base::{nil, id, YES},
    foundation::{NSString, NSArray},
};

mod clipboard;
mod notification;

#[link(name = "ScriptingBridge", kind = "framework")]
extern "C" {}

#[allow(unused)]
fn nsstring(s: &str) -> id {
    unsafe { NSString::alloc(nil).init_str(s) }
}

/// Get and print an objects description.
#[allow(unused)]
pub unsafe fn describe(obj: id) {
    let description: id = msg_send![obj, description];
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

unsafe fn list_processes() -> id {
    let s = nsstring("com.apple.systemevents");
    let system_events: id = msg_send![class!(SBApplication), applicationWithBundleIdentifier: s];
    msg_send![system_events, applicationProcesses]
}

unsafe fn filter(list: id, pred: id) -> id {
    msg_send![list, filteredArrayUsingPredicate: pred]
}

unsafe fn first(list: id) -> id {
    msg_send![list, objectAtIndex:0]
}

unsafe fn get(list: id, pred: id) -> id {
    first(filter(list, pred))
}

unsafe fn get_process(pred: id) -> id {
    get(list_processes(), pred)
}

#[allow(unused)]
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

unsafe fn click(item: id) -> id {
    msg_send![item, clickAt: nil]
}

macro_rules! f {
    ($obj:expr, $list_name:ident) => {{
        let list: id = msg_send![$obj, $list_name];
        first(list)
    }};
    ($obj:expr, $list_name:ident, name = $name:expr) => {{
        let list: id = msg_send![$obj, $list_name];
        msg_send![list, objectWithName: nsstring($name)]
    }};
}

unsafe fn frontmost_app() -> id {
    let fmt: id = nsstring("frontmost = YES");
    let pred: id = msg_send![class!(NSPredicate), predicateWithFormat: fmt];
    get_process(pred)
}

/// Return the bundle ID of the current (frontmost) app.
#[defun]
fn _get_current_app() -> Result<String> {
    autoreleasepool(|| unsafe {
        let frontmost = frontmost_app();
        let bundle: id = msg_send![frontmost, bundleIdentifier];
        Ok(to_str(bundle).expect("No frontmost application process was found").to_owned())
    })
}

/// Copy text from the app identified by BUNDLE-ID.
///
/// The copied text is put into the clipboard.
///
/// If BUNDLE-ID is nil, copy from the current (frontmost) app instead.
///
/// Return the bundle ID of the affected app. If the app is Emacs itself, return nil
/// without trying to copy.
///
/// If the app takes too long to copy the text, show a notification message to make
/// it feel less unresponsive.
#[defun]
fn _copy_text(bundle_id: Option<String>) -> Result<Option<String>> {
    let my_pid = process::id();
    Ok(autoreleasepool(|| unsafe {
        let process = match bundle_id {
            Some(bundle_id) => {
                let pred = bundle_id_is(&bundle_id);
                get_process(pred)
            }
            None => frontmost_app(),
        };
        let target_pid: u32 = msg_send![process, unixId];
        if target_pid == my_pid {
            return None;
        }
        // TODO: Allow the timeout to be customizable.
        let (center, ntf) = notification::schedule("Please wait!", "Copying ...", 1.0);
        let menu_bar: id = f![process, menuBars];
        let edit: id = f![menu_bar, menuBarItems, name = "Edit"];
        let menu: id = f![edit, menus];
        let select_all: id = f![menu, menuItems, name = "Select All"];
        let copy: id = f![menu, menuItems, name = "Copy"];
        click(select_all);
        click(copy);
        let bundle: id = msg_send![process, bundleIdentifier];
        notification::unschedule(center, ntf);
        Some(to_str(bundle).expect("Process has invalid bundle").to_owned())
    }))
}

/// Paste text from the clipboard into the app identified by BUNDLE-ID.
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
        click(select_all);
        click(paste);
    });
    Ok(())
}

/// Switch to the app identified by BUNDLE-ID (making it frontmost).
#[defun]
fn _activate_app(bundle_id: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        let pred = bundle_id_is(&bundle_id);
        // TODO: Get the app instead of getting its process through SystemEvents.
        let process = get_process(pred);
        msg_send![process, setFrontmost: YES];
    });
    Ok(())
}
