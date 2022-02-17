macro_rules! cb {
    () => {
        |_, _, _| ()
    };

    ($event:pat, $state:ident => $body:expr) => {
        move |$event, _: &mut _, $state: &mut $crate::state::State<_>| $body
    };
}

pub trait PseudoCell {
    type Contents;
    fn set(&self, value: Self::Contents);
    fn get(&self) -> Self::Contents
    where
        Self::Contents: Copy;
}

impl<T> PseudoCell for std::cell::Cell<T> {
    type Contents = T;
    fn set(&self, value: Self::Contents) {
        self.set(value)
    }
    fn get(&self) -> Self::Contents
    where
        Self::Contents: Copy,
    {
        self.get()
    }
}

impl<T> PseudoCell for std::cell::RefCell<T> {
    type Contents = T;
    fn set(&self, value: Self::Contents) {
        *self.borrow_mut() = value;
    }
    fn get(&self) -> Self::Contents
    where
        Self::Contents: Copy,
    {
        *self.borrow()
    }
}

impl<T> PseudoCell for std::sync::Mutex<T> {
    type Contents = T;
    fn set(&self, value: Self::Contents) {
        *self.lock().unwrap() = value;
    }
    fn get(&self) -> Self::Contents
    where
        Self::Contents: Copy,
    {
        *self.lock().unwrap()
    }
}
