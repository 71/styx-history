//! Automatically generates some files using Python, in order to avoid a hell of macros / compiler
//! plugins.

extern crate glob;

use std::env;
use std::process::{exit, Command};
use std::path::PathBuf;

use glob::glob;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    for file in glob("src/**/*.py").unwrap() {
        let path = match file {
            Ok(path) => path.canonicalize().unwrap(),
            Err(_)   => continue
        };

        let path = if cfg!(windows) {
            // Deceiving the borrow checker...
            match (unsafe { &*(&path as *const PathBuf) }).to_str() {
                Some(path) if path.starts_with("\\\\?\\") => {
                    PathBuf::from(&path[4..])
                },
                _ => path
            }
        } else {
            path
        };

        if format!("{}", path.display()).ends_with("common.py") {
            continue
        }

        exit(Command::new("python3")
            .current_dir(out_dir.as_str())
            .arg(path)
            .status()
            .expect("Could not run Python script to generate files.")
            .code()
            .unwrap());
    }
}
