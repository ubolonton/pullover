use std::{slice, str};

use cocoa::{
    base::{id, nil},
    foundation::NSString,
};

mod clipboard;
mod notification;
pub mod control;

#[allow(unused)]
fn nsstring(s: &str) -> id {
    unsafe { NSString::alloc(nil).init_str(s) }
}

/// Get and print an objects description.
#[allow(unused)]
pub unsafe fn describe(obj: id) {
    let description: id = msg_send![obj, description];
    if let Some(desc_str) = to_str(description) {
        eprintln!("    : {}", desc_str);
    } else {
        eprintln!("    : nil");
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
