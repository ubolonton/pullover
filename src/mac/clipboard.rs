use emacs::{defun, Result, Env};

use std::time::{Duration, Instant};

use objc::rc::autoreleasepool;
use cocoa::{
    base::nil,
    appkit::NSPasteboard,
};

// TODO: This should be in the `emacs` crate.
/// This should be used instead of thread::sleep, so that the GIL is released for the duration of
/// the sleep, enabling other Lisp threads to run.
fn sleep(env: &Env, seconds: f64) -> Result<()> {
    env.call("sleep-for", (seconds,))?;
    Ok(())
}

/// Return the current "changeCount" of the clipboard.
#[defun]
fn _change_count() -> Result<i64> {
    autoreleasepool(|| {
        Ok(unsafe { NSPasteboard::generalPasteboard(nil).changeCount() })
    })
}

/// Repeatedly poll the clipboard for new items, returning the "changeCount" of the clipboard. If no
/// item is copied to the clipboard after the given number of milliseconds, return nil.
///
/// This should be run in a background thread.
#[defun]
fn _wait_for_clipboard(env: &Env, timeout: u64, count: Option<i64>) -> Result<Option<i64>> {
    autoreleasepool(|| {
        let pb = unsafe { NSPasteboard::generalPasteboard(nil) };
        let start = Instant::now();
        let count = count.unwrap_or_else(|| unsafe { pb.changeCount() });
        let timeout = Duration::from_millis(timeout);
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
    })
}
