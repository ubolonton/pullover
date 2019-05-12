#[macro_use]
extern crate objc;

use emacs::{Env, Result};

pub mod mac;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(mod_in_name = false)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
