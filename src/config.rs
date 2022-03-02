use std::{collections::HashMap, io::ErrorKind};

use once_cell::sync::OnceCell;
use serde::Deserialize;
use slog::{error, info, warn, Logger};

#[non_exhaustive]
#[derive(Deserialize, Default)]
pub struct Config {
    #[serde(default, rename = "display")]
    pub displays: HashMap<String, Display>,
    #[serde(default)]
    pub xcursor: XCursorConfig,
}

impl Config {
    pub fn display_mode(&self, display: &str) -> Option<Mode> {
        self.displays.get(display).and_then(|d| d.mode)
    }

    pub fn display_scale(&self, display: &str) -> f64 {
        self.displays.get(display).map_or(1.0, |d| d.scale)
    }
}

#[non_exhaustive]
#[derive(Deserialize, Clone, Default)]
pub struct Display {
    pub mode: Option<Mode>,
    #[serde(default = "default_scale")]
    pub scale: f64,
}

fn default_scale() -> f64 {
    1.0
}

#[non_exhaustive]
#[derive(Copy, Clone)]
pub struct Resolution {
    pub x: u16,
    pub y: u16,
}

#[non_exhaustive]
#[derive(Deserialize, Copy, Clone)]
#[serde(from = "(u16, u16, f32)")]
pub struct Mode {
    pub resolution: Resolution,
    pub framerate: f32,
}

impl From<(u16, u16, f32)> for Mode {
    fn from((x, y, framerate): (u16, u16, f32)) -> Self {
        Self {
            resolution: Resolution { x, y },
            framerate,
        }
    }
}

impl From<Mode> for smithay::wayland::output::Mode {
    fn from(mode: Mode) -> Self {
        Self {
            size: (mode.resolution.x as i32, mode.resolution.y as i32).into(),
            refresh: (mode.framerate * 1000.) as i32,
        }
    }
}

#[non_exhaustive]
#[derive(Deserialize, Clone)]
pub struct XCursorConfig {
    #[serde(default = "default_xcursor_theme")]
    pub theme: String,
    #[serde(default = "default_xcursor_size")]
    pub size: u32,
}

fn default_xcursor_theme() -> String {
    std::env::var("XCURSOR_THEME").unwrap_or_else(|_| "default".into())
}

fn default_xcursor_size() -> u32 {
    std::env::var("XCURSOR_SIZE")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(24)
}

impl Default for XCursorConfig {
    fn default() -> Self {
        Self {
            theme: default_xcursor_theme(),
            size: default_xcursor_size(),
        }
    }
}

static CONFIG: OnceCell<Config> = OnceCell::new();

fn read_config(log: &Logger) -> Option<Config> {
    let name = crate::NAME;

    let path = match dirs::config_dir() {
        Some(dir) => dir.join(name).join(name).with_extension("toml"),
        None => {
            error!(log, "Could not determine config dir; using defaults");
            return None;
        }
    };

    let data = match std::fs::read(&path) {
        Ok(data) => data,
        Err(e) => {
            if e.kind() == ErrorKind::NotFound {
                warn!(
                    log,
                    "Did not find config file; using defaults";
                    "path" => path.display(),
                );
            } else {
                error!(
                    log,
                    "Could not read config file; using defaults";
                    "path" => path.display(),
                    "error" => e,
                );
            }
            return None;
        }
    };

    let config = match toml::from_slice(&data) {
        Ok(cfg) => cfg,
        Err(e) => {
            error!(log, "Could not deserialize config; using defaults"; "error" => e.to_string());
            return None;
        }
    };
    info!(log, "Successfully loaded config");
    Some(config)
}

pub fn init(log: &Logger) {
    let cfg = read_config(log).unwrap_or_default();
    CONFIG.set(cfg).ok().unwrap();
}

pub fn get() -> &'static Config {
    CONFIG.get().unwrap()
}
