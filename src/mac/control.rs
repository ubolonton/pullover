use emacs::{defun, Result};

use std::{
    process,
    time::Instant,
};

use objc::rc::autoreleasepool;
use cocoa::{
    base::{nil, id, YES},
    foundation::{NSArray},
};

use super::*;

#[link(name = "ScriptingBridge", kind = "framework")]
extern "C" {}

unsafe fn list_processes() -> id {
    let s = nsstring("com.apple.systemevents");
    let system_events: id = msg_send![class!(SBApplication), applicationWithBundleIdentifier: s];
    msg_send![system_events, applicationProcesses]
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
    Ok(copy_text(bundle_id, None))
}

pub fn copy_text(bundle_id: Option<String>, process_id: Option<u32>) -> Option<String> {
    let process_id = process_id.unwrap_or_else(|| process::id());
    autoreleasepool(|| unsafe {
        let t = Instant::now();
        let process = match bundle_id {
            Some(bundle_id) => {
                let pred = bundle_id_is(&bundle_id);
                get_process(pred)
            }
            None => frontmost_app(),
        };
        eprintln!("{:?} process", t.elapsed());
        let process: id = msg_send![process, get];
        eprintln!("{:?} process get", t.elapsed());
        let target_pid: u32 = msg_send![process, unixId];
        eprintln!("{:?} PID", t.elapsed());
        eprintln!("{} - {}", process_id, target_pid);
        if target_pid == process_id {
            return None;
        }
        // TODO: Allow the timeout to be customizable.
        let (center, ntf) = notification::schedule("Please wait!", "Copying ...", 1.0);
        eprintln!("{:?} notify", t.elapsed());
        let menu_bar: id = f![process, menuBars];
        eprintln!("{:?} menu_bar", t.elapsed());
        let edit: id = f![menu_bar, menuBarItems, name = "Edit"];
        eprintln!("{:?} edit", t.elapsed());
        let menu: id = f![edit, menus];
        //        let menu: id = msg_send![menu, get];
        eprintln!("{:?} menu", t.elapsed());
        let select_all: id = f![menu, menuItems, name = "Select All"];
        eprintln!("{:?} select_all", t.elapsed());
        let copy: id = f![menu, menuItems, name = "Copy"];
        eprintln!("{:?} copy", t.elapsed());
        click(select_all);
        eprintln!("{:?} click select_all", t.elapsed());
        click(copy);
        eprintln!("{:?} click copy", t.elapsed());
        let bundle: id = msg_send![process, bundleIdentifier];
        eprintln!("{:?} bundle", t.elapsed());
        notification::unschedule(center, ntf);
        Some(to_str(bundle).expect("Process has invalid bundle").to_owned())
    })
}

/// Paste text from the clipboard into the app identified by BUNDLE-ID.
#[defun]
fn _paste_text(bundle_id: String) -> Result<()> {
    paste_text(&bundle_id);
    Ok(())
}

pub fn paste_text(bundle_id: &str) {
    autoreleasepool(|| unsafe {
        eprintln!("bundle_id {}", bundle_id);
        let t = Instant::now();
        let pred = bundle_id_is(bundle_id);
        let process = get_process(pred);
        eprintln!("{:?} process", t.elapsed());
        let process: id = msg_send![process, get];
        eprintln!("{:?} process get", t.elapsed());
        describe(process);
        msg_send![process, setFrontmost: YES];
        eprintln!("{:?} activated", t.elapsed());
        let menu_bar: id = f![process, menuBars];
        eprintln!("{:?} menu_bar", t.elapsed());
        let edit: id = f![menu_bar, menuBarItems, name = "Edit"];
        eprintln!("{:?} edit", t.elapsed());
        let menu: id = f![edit, menus];
        eprintln!("{:?} menu", t.elapsed());
        let select_all: id = f![menu, menuItems, name = "Select All"];
        eprintln!("{:?} select_all", t.elapsed());
        let paste: id = f![menu, menuItems, name = "Paste"];
        eprintln!("{:?} paste", t.elapsed());
        click(select_all);
        eprintln!("{:?} clicked select_all", t.elapsed());
        click(paste);
        eprintln!("{:?} clicked paste", t.elapsed());
    })
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
