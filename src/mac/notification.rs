use emacs::{defun, Result};

use objc::rc::autoreleasepool;
use cocoa::base::id;
use std::time::Instant;

use super::nsstring;

#[link(name = "Foundation", kind = "framework")]
#[allow(unused)]
extern "C" {
    pub static NSUserNotificationDefaultSoundName: id;
}

pub unsafe fn schedule(title: &str, text: &str, delay_secs: f64) -> (id, id) {
    // XXX: Use the new UNUserNotificationCenter once it's less buggy. See
    // https://stackoverflow.com/a/49559863
    // https://stackoverflow.com/questions/47255156/crash-invalid-parameter-not-satisfying-bundleproxy-nil-unusernotificatio?noredirect=1&lq=1
    let center: id = msg_send![class!(NSUserNotificationCenter), defaultUserNotificationCenter];
    let ntf: id = msg_send![class!(NSUserNotification), alloc];
    let ntf: id = msg_send![ntf, init];
    let identifier = &format!("{:?}", Instant::now());
    msg_send![ntf, setIdentifier: nsstring(identifier)];
    msg_send![ntf, setTitle: nsstring(title)];
    msg_send![ntf, setInformativeText: nsstring(text)];
    let ts: id = msg_send![class!(NSDate), dateWithTimeIntervalSinceNow: delay_secs];
    msg_send![ntf, setDeliveryDate: ts];

    msg_send![center, scheduleNotification: ntf];

    (center, msg_send![ntf, autorelease])
}

pub unsafe fn unschedule(notification_center: id, notification: id) -> id {
    msg_send![notification_center, removeScheduledNotification:notification]
}

#[defun]
fn _notify(title: String, text: String) -> Result<()> {
    autoreleasepool(|| unsafe {
        schedule(&title, &text, 0.0);
    });
    Ok(())
}
