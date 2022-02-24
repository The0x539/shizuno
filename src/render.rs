use smithay::backend::{
    renderer::{
        gles2::{Gles2Frame, Gles2Renderer},
        Frame,
    },
    SwapBuffersError,
};
use smithay::utils::{Logical, Rectangle};
use smithay::wayland::shell::wlr_layer::Layer;

use slog::Logger;

use crate::{
    drawing::{draw_layers, draw_windows},
    window_map::WindowMap,
};

pub fn render_layers_and_windows(
    renderer: &mut Gles2Renderer,
    frame: &mut Gles2Frame,
    window_map: &WindowMap,
    geometry: Rectangle<i32, Logical>,
    scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    macro_rules! r_f {
        () => {
            (&mut *renderer, &mut *frame)
        };
    }

    frame.clear([0.8, 0.8, 0.9, 1.0], None)?;

    draw_layers(r_f!(), window_map, Layer::Background, geometry, scale, log)?;
    draw_layers(r_f!(), window_map, Layer::Bottom, geometry, scale, log)?;
    draw_windows(r_f!(), window_map, geometry, scale, log)?;
    draw_layers(r_f!(), window_map, Layer::Top, geometry, scale, log)?;
    draw_layers(r_f!(), window_map, Layer::Overlay, geometry, scale, log)?;

    Ok(())
}
