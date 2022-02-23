#![deny(rust_2018_idioms)]

use std::sync::Mutex;

use slog::{Drain, Logger};

fn main() {
    let drain = Mutex::new(slog_term::term_full()).fuse();
    let log = Logger::root(drain, slog::o!());

    wm::udev::run(log);
}
