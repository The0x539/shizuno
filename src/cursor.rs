use std::{rc::Rc, time::Duration};

use slog::Logger;

use xcursor::parser::Image;

pub struct Cursor;

impl Cursor {
    pub fn load(_log: &Logger) -> Self {
        todo!()
    }

    pub fn get_image(&self, _scale: i32, _time: Duration) -> Rc<Image> {
        todo!()
    }
}
