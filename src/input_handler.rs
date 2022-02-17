use smithay::backend::input::{InputBackend, InputEvent};

use crate::{state::State, udev::UdevData};

impl State<UdevData> {
    pub fn process_input_event<B: InputBackend>(&mut self, _event: InputEvent<B>) {
        todo!()
    }
}
