#![deny(rust_2018_idioms)]

pub const NAME: &str = "shizuno";

#[macro_use]
mod util;

pub mod config;
mod cursor;
mod drawing;
mod input_handler;
mod render;
mod shell;
mod state;
pub mod udev;
mod xwayland;
