#![deny(rust_2018_idioms)]

use slog::{Drain, Logger};

fn main() {
    let drain = slog_journald::JournaldDrain.fuse();
    let o = slog::o!("SYSLOG_IDENTIFIER" => shizuno::NAME);
    let log = Logger::root(drain, o);

    shizuno::config::init(&log);

    shizuno::udev::run(log);
}
