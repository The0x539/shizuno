use smithay::reexports::*;
use smithay::utils::{Logical, Rectangle};

use smithay::wayland::output::{Mode, PhysicalProperties};
use wayland_commons::user_data::UserDataMap;

pub struct OutputMap;

impl OutputMap {
    pub fn add<N: AsRef<str>>(
        &mut self,
        _name: N,
        _props: PhysicalProperties,
        _mode: Mode,
    ) -> &Output {
        todo!()
    }

    pub fn retain<F: FnMut(&Output) -> bool>(&mut self, _f: F) {
        todo!()
    }

    pub fn find<F: FnMut(&&Output) -> bool>(&self, _f: F) -> Option<&Output> {
        todo!()
    }

    pub fn refresh(&mut self) {
        todo!()
    }
}

pub struct Output;

impl Output {
    pub fn userdata(&self) -> &UserDataMap {
        todo!()
    }

    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        todo!()
    }

    pub fn scale(&self) -> f32 {
        todo!()
    }

    pub fn current_mode(&self) -> Mode {
        todo!()
    }
}
