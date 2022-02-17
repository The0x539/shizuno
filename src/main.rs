#![deny(rust_2018_idioms)]

use std::sync::Mutex;

use slog::{Drain, Logger};

#[macro_use]
mod util;

mod cursor;
mod drawing;
mod input_handler;
mod output_map;
mod shell;
mod state;
mod udev;
mod window_map;
mod xwayland;

fn main() {
    let drain = Mutex::new(slog_term::term_full()).fuse();
    let log = Logger::root(drain, slog::o!());

    udev::run(log);
}
