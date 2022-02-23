#![deny(rust_2018_idioms)]

use slog::{Drain, Logger};

fn main() {
    let drain = slog_async::Async::default(slog_term::term_full().fuse()).fuse();
    let log = Logger::root(drain, slog::o!());

    wm::udev::run(log);
}
