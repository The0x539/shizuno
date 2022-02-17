use smithay::backend::{
    renderer::{
        gles2::{Gles2Error, Gles2Renderer, Gles2Texture},
        Frame, ImportAll, Renderer, Texture,
    },
    SwapBuffersError,
};
use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle};

use image::{ImageBuffer, Rgba};
use slog::Logger;
use wayland_server::protocol::wl_surface::WlSurface;

use crate::window_map::WindowMap;

// TODO: a more general "draw context bundle" kinda struct.
// these functions' signatures are very similar.
pub trait RendererAndFrame<'r, 'f> {
    type R: Renderer<Error = Self::E, TextureId = Self::T, Frame = Self::F> + ImportAll;
    type F: Frame<Error = Self::E, TextureId = Self::T>;
    type E: std::error::Error + Into<SwapBuffersError>;
    type T: Texture + 'static;
    fn pair(self) -> (&'r mut Self::R, &'f mut Self::F);
}

impl<'r, 'f, R, E, F, T> RendererAndFrame<'r, 'f> for (&'r mut R, &'f mut F)
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    E: std::error::Error + Into<SwapBuffersError>,
    T: Texture + 'static,
{
    type R = R;
    type F = F;
    type E = E;
    type T = T;
    fn pair(self) -> (&'r mut R, &'f mut F) {
        self
    }
}

pub fn draw_cursor<'r, 'e>(
    _r_f: impl RendererAndFrame<'r, 'e>,
    _surface: &WlSurface,
    _location: Point<i32, Logical>,
    _output_scale: f32,
    _log: &Logger,
) -> Result<(), SwapBuffersError> {
    todo!()
}

pub fn draw_drag_icon<'r, 'e>(
    _r_f: impl RendererAndFrame<'r, 'e>,
    _surface: &WlSurface,
    _location: Point<i32, Logical>,
    _output_scale: f32,
    _log: &Logger,
) -> Result<(), SwapBuffersError> {
    todo!()
}

pub fn draw_windows<'r, 'e>(
    _r_f: impl RendererAndFrame<'r, 'e>,
    _window_map: &WindowMap,
    _output_rect: Rectangle<i32, Logical>,
    _output_scale: f32,
    _log: &Logger,
) -> Result<(), SwapBuffersError> {
    todo!()
}

#[cfg(feature = "debug")]
pub fn draw_fps<'r, 'e, RF: RendererAndFrame<'r, 'e>>(
    _r_f: RF,
    _texture: &RF::T,
    _output_scale: f64,
    _value: u32,
) -> Result<(), SwapBuffersError> {
    todo!()
}

pub fn import_bitmap(
    renderer: &mut Gles2Renderer,
    image: &ImageBuffer<Rgba<u8>, &[u8]>,
) -> Result<Gles2Texture, Gles2Error> {
    use smithay::backend::renderer::gles2::ffi;

    let (w, h) = (image.width() as i32, image.height() as i32);
    let clamp = ffi::CLAMP_TO_EDGE as i32;

    renderer.with_context(|renderer, gl| unsafe {
        let mut tex = 0;
        gl.GenTextures(1, &mut tex);
        gl.BindTexture(ffi::TEXTURE_2D, tex);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, clamp);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, clamp);
        gl.TexImage2D(
            ffi::TEXTURE_2D,
            0,
            ffi::RGBA as i32,
            w,
            h,
            0,
            ffi::RGBA,
            ffi::UNSIGNED_BYTE as u32,
            image.as_ptr() as *const _,
        );
        gl.BindTexture(ffi::TEXTURE_2D, 0);

        Gles2Texture::from_raw(renderer, tex, (w, h).into())
    })
}
