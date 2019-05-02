use std::thread;
use std::time::{Duration, Instant};

use emacs::{defun, Result, Env, IntoLisp};

use objc::runtime::{Class, Object};
use cocoa::appkit::NSPasteboard;
use cocoa::base::{nil, id};
use cocoa::foundation::NSArray;

// TODO: This should be in the `emacs` crate.
/// This should be used instead of thread::sleep, so that the GIL is released for the duration of
/// the sleep, enabling other Lisp threads to run.
fn sleep(env: &Env, seconds: f64) -> Result<()> {
    env.call("sleep-for", &[seconds.into_lisp(env)?])?;
    Ok(())
}

/// Repeatedly poll the clipboard for new items, returning the "changeCount" of the clipboard. If no
/// item is copied to the clipboard after the given number of milliseconds, return nil.
///
/// This should be run in a background thread.
#[defun]
fn watch_clipboard(env: &Env, timeout: i64) -> Result<Option<i64>> {
    let pb = unsafe { NSPasteboard::generalPasteboard(nil) };
    let count = unsafe { pb.changeCount() };
    let start = Instant::now();
    let timeout = Duration::from_millis(timeout as u64);
    loop {
        sleep(env, 0.02)?;
        let new_count = unsafe { pb.changeCount() };
        if new_count != count {
            return Ok(Some(new_count));
        }
        if start.elapsed() > timeout {
            return Ok(None);
        }
    }
}

#[defun]
fn sleep_n(env: &Env, seconds: f64) -> Result<()> {
    env.message("pre-yield")?;
    env.call("thread-yield", &[])?;
    env.message("post-yield")?;
    thread::sleep(Duration::from_millis((seconds * 1000.0) as u64));
    env.message("post-sleep")?;
    Ok(())
}

#[defun]
fn sleep_e(env: &Env, seconds: f64) -> Result<()> {
    env.message("pre-yield")?;
    env.call("thread-yield", &[])?;
    env.message("post-yield")?;
    env.call("sleep-for", &[seconds.into_lisp(env)?])?;
    env.message("post-sleep")?;
    Ok(())
}
