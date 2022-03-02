use std::time::Duration;

use slog::{warn, Logger};

use xcursor::{
    parser::{parse_xcursor, Image},
    CursorTheme,
};

static FALLBACK_CURSOR_DATA: &[u8] = include_bytes!("../resources/cursor.rgba");

pub struct Cursor {
    icons: Box<[Image]>,
    size: u32,
}

impl Cursor {
    pub fn load(log: &Logger) -> Self {
        let cfg = &crate::config::get().xcursor;
        let (name, size) = (&cfg.theme, cfg.size);

        let theme = CursorTheme::load(name);
        let icons = match load_icon(&theme) {
            Ok(theme) => theme,
            Err(e) => {
                warn!(log, "Unable to load xcursor: {e}; using fallback cursor");
                Box::new([Image {
                    size: 32,
                    width: 64,
                    height: 64,
                    xhot: 1,
                    yhot: 1,
                    delay: 1,
                    pixels_rgba: FALLBACK_CURSOR_DATA.to_vec(),
                    pixels_argb: vec![],
                }])
            }
        };

        Self { icons, size }
    }

    pub fn get_image(&self, scale: u32, time: Duration) -> &Image {
        frame(time, self.size * scale, &self.icons)
    }
}

fn nearest_images(size: u32, images: &[Image]) -> impl Iterator<Item = &Image> {
    let nearest = images.iter().min_by_key(|i| i.size.abs_diff(size)).unwrap();
    let dims = (nearest.width, nearest.height);
    images.iter().filter(move |i| (i.width, i.height) == dims)
}

fn frame(time: Duration, size: u32, images: &[Image]) -> &Image {
    let total = nearest_images(size, images).map(|i| i.delay).sum::<u32>();
    let mut millis = time.as_millis() as u32 % total;

    for img in nearest_images(size, images) {
        if millis < img.delay {
            return img;
        }
        millis -= img.delay;
    }

    unreachable!()
}

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("Theme has no default cursor")]
    NoDefaultCursor,
    #[error("Error opening xcursor file: {0}")]
    File(#[from] std::io::Error),
    #[error("Failed to parse XCursor file")]
    Parse,
}

fn load_icon(theme: &CursorTheme) -> Result<Box<[Image]>, Error> {
    let icon_path = theme.load_icon("default").ok_or(Error::NoDefaultCursor)?;
    let cursor_data = std::fs::read(&icon_path)?;
    let images = parse_xcursor(&cursor_data).ok_or(Error::Parse)?;
    Ok(images.into_boxed_slice())
}
