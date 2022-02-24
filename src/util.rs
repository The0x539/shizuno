use smithay::reexports::*;
use smithay::wayland::compositor::SurfaceData;

use wayland_server::protocol::wl_surface::WlSurface;

macro_rules! cb {
    () => {
        |_, _, _| ()
    };

    ($event:pat, $state:pat => $body:expr) => {
        move |$event, _: &mut _, $state: &mut $crate::state::State<_>| $body
    };
}

macro_rules! try_or {
    ($or:expr, $try:expr $(,)?) => {
        match $try {
            Some(v) => v,
            None => $or,
        }
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

pub fn with_surface_tree_upward_all(
    wl_surface: &WlSurface,
    mut f: impl FnMut(&WlSurface, &SurfaceData),
) {
    smithay::wayland::compositor::with_surface_tree_upward(
        wl_surface,
        (),
        |_, _, &()| smithay::wayland::compositor::TraversalAction::DoChildren(()),
        |wl_surface, states, &()| f(wl_surface, states),
        |_, _, &()| true,
    );
}
