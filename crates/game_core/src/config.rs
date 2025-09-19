#![coverage(off)]

use std::fs::{read_to_string, write};
use std::path::Path;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use crate::key_converter::convert;
// =================================================================================================
//
//                                            Global
//
// =================================================================================================

#[derive(Resource, Deserialize, Serialize, Clone, Debug, Default)]
pub struct GlobalConfig {
    pub graphics_config: GraphicsConfig,
    pub input_config: InputConfig,
}

impl GlobalConfig {

    /// Loads a configuration file and deserializes it into the specified type.
    ///
    /// # Arguments
    /// - `path`: The file path of the configuration file to load.
    ///
    /// # Panics
    /// This function will panic if the file cannot be read or parsed correctly.
    ///
    /// # Returns
    /// - `T`: The deserialized configuration data.
    pub fn load<T: for<'de> Deserialize<'de>>(path: &str) -> T {
        let content = read_to_string(Path::new(path)).expect("Failed to read config file");
        toml::from_str(&content).expect("Failed to parse toml file")
    }

    /// Creates a new `GlobalConfig` instance and loads all configuration files.
    ///
    ///
    /// # Returns
    /// - `GlobalConfig`: A new instance with loaded configurations for game, graphics, input, and audio.
    pub fn new() -> Self {
        Self {
            graphics_config: Self::load("config/graphics.toml"),
            input_config: Self::load("config/input.toml"),
        }
    }

    /// Saves a specified file with his name.
    fn save<T: Serialize>(data: &T, path: &str) {
        let toml_string = toml::to_string_pretty(data).expect("Failed to serialize to TOML");
        write(Path::new(path), toml_string).expect("Failed to write config file");
    }

    /// Saves all known config files which found in config/ folder.
    /// This func used `GlobalConfig::save` for saving.
    pub fn save_all(&self) {
        Self::save(&self.graphics_config, "config/graphics.toml");
    }

}

// =================================================================================================
//
//                                            Graphics
//
// =================================================================================================

#[derive(Resource, Deserialize, Serialize, Clone, Debug)]
pub struct GraphicsConfig {
    pub window_resolution: String,

    pub fullscreen: bool,
    pub vsync: bool,

    pub video_backend: String,
    pub chunk_range: u8,
    #[serde(default)]
    pub texture_quality: TextureQuality,
}

impl Default for GraphicsConfig {
    fn default() -> Self {
        Self {
            window_resolution: String::from("1270x720"),
            fullscreen: false,
            vsync: true,
            video_backend: String::from("AUTO"),
            chunk_range: 8,
            texture_quality: TextureQuality::default()
        }
    }
}

impl GraphicsConfig {
    pub fn get_window_width(&self) -> f32 {
        let (width, _) = parse_resolution(self.window_resolution.as_str())
            .unwrap_or_else(|_| (1280.0, 720.0));
        width
    }

    pub fn get_window_height(&self) -> f32 {
        let (_, height) = parse_resolution(self.window_resolution.as_str())
            .unwrap_or_else(|_| (1280.0, 720.0));
        height
    }
}

#[derive(Deserialize, Serialize, Default, Debug, Clone, Copy, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum TextureQuality {
    #[default]
    #[serde(alias = "Low")]
    Low,
    #[serde(alias = "Medium")]
    Medium,
    #[serde(alias = "High")]
    High,
    #[serde(alias = "Max")]
    Max,
}

impl TextureQuality {
    /// Map resolution to tile size in pixels.
    pub const fn tile_px(self) -> u16 {
        match self {
            TextureQuality::Low    => 64,
            TextureQuality::Medium => 128,
            TextureQuality::High   => 256,
            TextureQuality::Max    => 512,
        }
    }
}

// =================================================================================================
//
//                                            Input
//
// =================================================================================================

#[derive(Resource, Deserialize, Serialize, Clone, Debug)]
pub struct InputConfig {
    pub inspector: String
}

impl Default for InputConfig {
    fn default() -> Self {
        Self {
            inspector: String::from("F1"),
        }
    }
}

impl InputConfig {
    pub fn get_inspector_key(&self) -> KeyCode {
        convert(self.inspector.as_str()).unwrap_or_else(|| KeyCode::F12)
    }

}

// =================================================================================================
//
//                                         Internal Func
//
// =================================================================================================

#[inline]
fn parse_resolution(s: &str) -> Result<(f32, f32), String> {
    let (w_str, h_str) = s
        .trim()
        .split_once(['x', 'X'])
        .ok_or_else(|| format!("Wrong Format: '{}'. Example z. B. 1280x720", s))?;

    let w: f32 = w_str.trim().parse()
        .map_err(|_| format!("Width is not a number: '{}'", w_str.trim()))?;
    let h: f32 = h_str.trim().parse()
        .map_err(|_| format!("Height is not a number: '{}'", h_str.trim()))?;

    if w <= 0.0 || h <= 0.0 {
        return Err("Width / Height needs a positive number like > 0".into());
    }
    Ok((w, h))
}
