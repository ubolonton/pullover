#[macro_use]
extern crate objc;

use std::env;

mod mac;

// TODO: Put the core logic into a sub-crate, instead of having 2 targets.
#[allow(unused)]
fn main() {
    let args: Vec<String> = env::args().collect();
    let cmd = &args[1];
    match cmd.as_ref() {
        "copy-text" => {
            let process_id = *&args[2].replace("\n", "").parse::<u32>().unwrap();
            let app = mac::control::copy_text(None, Some(process_id));
            let app = match &app {
                Some(app) => &app.bundle_id,
                None => "",
            };
            print!("{}", app);
        }
        "paste-text" => {
            // XXX: We need to nuke shell-added newline, and emacsclient-added double quotes.
            let app = &args[2].replace("\n", "").replace("\"", "");
            mac::control::paste_text(app);
        }
        cmd => {
            panic!("Unknown command {}", cmd);
        }
    }
}
