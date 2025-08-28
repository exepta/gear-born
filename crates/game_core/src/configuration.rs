#![coverage(off)]

use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Top‐level game configuration resource.
///
/// This resource aggregates all configurable subsystems of the game,
/// including graphics, input, and audio settings. It can be
/// deserialized from and serialized to external configuration files
/// and is registered as a Bevy `Resource`.
#[derive(Resource, Deserialize, Serialize, Debug, Clone)]
pub struct GameConfig {
    /// Settings related to rendering and display.
    pub graphics: GraphicsConfig,

    /// Settings related to user input mappings and sensitivities.
    pub input: InputConfig,

    /// Settings related to sound volumes and audio device preferences.
    pub audio: AudioConfig,
}

impl Default for GameConfig {
    fn default() -> Self {
        Self {
            graphics: GraphicsConfig::default(),
            input: InputConfig::default(),
            audio: AudioConfig::default()
        }
    }
}

impl GameConfig {

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
        let content = fs::read_to_string(Path::new(path)).expect("Failed to read config file");
        toml::from_str(&content).expect("Failed to parse toml file")
    }

    /// Creates a new `ConfigService` instance and loads all configuration files.
    ///
    ///
    /// # Returns
    /// - `ConfigService`: A new instance with loaded configurations for game, graphics, input, and audio.
    pub fn new() -> Self {
        Self {
            graphics: Self::load("config/graphicsConfig.toml"),
            input: Self::load("config/gameInput.toml"),
            audio: Self::load("config/gameAudio.toml"),
        }
    }

    fn save<T: Serialize>(data: &T, path: &str) {
        let toml_string = toml::to_string_pretty(data).expect("Failed to serialize to TOML");
        fs::write(Path::new(path), toml_string).expect("Failed to write config file");
    }

    pub fn save_all(&self) {
        Self::save(&self.graphics, "config/graphicsConfig.toml");
        Self::save(&self.input, "config/gameInput.toml");
        Self::save(&self.audio, "config/gameAudio.toml");
    }
}

/// Configuration settings for the graphics subsystem.
///
/// This struct defines window dimensions, display modes, and rendering backend
/// preferences. It can be serialized to or deserialized from external configuration
/// files to customize the game’s graphical behavior.
#[derive(Deserialize, Serialize, Debug, Clone)]
#[allow(dead_code)]
pub struct GraphicsConfig {
    /// The width of the application window (in logical pixels or units).
    pub window_width: f32,

    /// The height of the application window (in logical pixels or units).
    pub window_height: f32,

    /// Whether the application should run in fullscreen mode.
    pub fullscreen: bool,

    /// Whether vertical synchronization (vsync) is enabled.
    pub vsync: bool,

    /// Identifier for the graphics backend to use (e.g., "wgpu", "OpenGL", "Vulkan").
    pub graphic_backend: String,
    
    pub chunk_range: i32
}

impl Default for GraphicsConfig {
    fn default() -> Self {
        Self {
            window_width: 1270.0,
            window_height: 720.0,
            fullscreen: false,
            vsync: true,
            graphic_backend: String::from("AUTO"),
            chunk_range: 8
        }
    }
}

/// Configuration settings for user input and control mappings.
///
/// This struct defines sensitivity parameters for camera controls,
/// keybindings for player actions, character swapping, world combat,
/// and UI navigation. It can be deserialized from and serialized to
/// external configuration files to allow users to customize their
/// control scheme.
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct InputConfig {
    // Camera

    /// Vertical sensitivity for mouse movement when controlling the camera.
    pub mouse_sensitivity_vertical: f32,

    /// Horizontal sensitivity for mouse movement when controlling the camera.
    pub mouse_sensitivity_horizontal: f32,

    /// Amount to zoom in per scroll unit or mouse wheel step.
    pub mouse_zoom_in: f32,

    /// Amount to zoom out per scroll unit or mouse wheel step.
    pub mouse_zoom_out: f32,

    /// Key or button mapping used to unlock or free the mouse cursor from the window.
    pub mouse_screen_unlock: String,

    // Player

    /// Key or button mapping for moving the player character upward.
    pub move_up: String,

    /// Key or button mapping for moving the player character downward.
    pub move_down: String,

    /// Key or button mapping for moving the player character to the left.
    pub move_left: String,

    /// Key or button mapping for moving the player character to the right.
    pub move_right: String,

    /// Key or button mapping for making the player character jump.
    pub jump: String,

    /// Key or button mapping for making the player character sprint.
    pub sprint: String,

    /// Key or button mapping for interacting with objects or NPCs.
    pub interact: String,

    /// Key or button mapping for performing a standard world attack.
    pub attack: String,

    // UI

    /// Key or button mapping to open or toggle the in‐game menu.
    pub ui_menu: String,

    /// Key or button mapping to open or toggle the inventory screen.
    pub ui_inventory: String,

    /// Key or button mapping to close UI dialogs or go back in menus.
    pub ui_close_back: String,

    // Debug

    /// Toggle Rapier 3d grid layout.
    pub rapier_debug: String,

    /// Toggle world inspector.
    pub world_inspector: String,
}

impl Default for InputConfig {
    fn default() -> Self {
        Self {
            mouse_sensitivity_vertical: 1.0,
            mouse_sensitivity_horizontal: 1.0,
            mouse_zoom_in: 3.5,
            mouse_zoom_out: 10.0,
            mouse_screen_unlock: String::from("AltLeft"),

            move_up: String::from("W"),
            move_down: String::from("S"),
            move_left: String::from("A"),
            move_right: String::from("D"),
            jump: String::from("Space"),
            sprint: String::from("ShiftLeft"),
            interact: String::from("F"),

            attack: String::from("MouseLeft"),
            ui_menu: String::from("Escape"),
            ui_inventory: String::from("B"),
            ui_close_back: String::from("Escape"),

            rapier_debug: String::from("F3"),
            world_inspector: String::from("F1"),
        }
    }

}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct AudioConfig {
    pub master_volume: f32,
    pub environment_volume: f32,
    pub speak_volume: f32,
    pub sfx_volume: f32,
    pub ui_volume: f32
}

impl Default for AudioConfig {
    fn default() -> Self {
        Self {
            master_volume: 1.0,
            environment_volume: 1.0,
            speak_volume: 1.0,
            sfx_volume: 1.0,
            ui_volume: 1.0
        }
    }
}

#[derive(Resource, Clone)]
pub struct WorldGenConfig {
    pub seed: i32,
    pub enable_caves: bool,

    pub base_height: i32,
    pub height_span: i32,
    pub height_freq: f32,
    pub warp_freq: f32,
    pub warp_amp: f32,

    pub caves_freq: f32,
    pub caves_thresh: f32,

    pub cave_min_y: i32,
    pub cave_max_y: i32,

    pub caves_tunnel_abs_pow: f32,

    pub caverns_freq: f32,
    pub caverns_thresh: f32,
    pub caverns_weight: f32,

    pub surface_shell: i32,
    pub entrance_depth: i32,
    pub entrance_bonus: f32,
    pub entrance_chance_freq: f32,
    pub entrance_chance_thresh: f32,

    pub entrance_open_depth: i32,
    pub entrance_open_radius: i32,

    pub caves_region_freq: f32,
    pub caves_region_thresh: f32,

    pub cave_y_scale: f32,

    pub plains_freq: f32,
    pub plains_threshold: f32,
    pub plains_blend: f32,
    pub plains_span: i32,
    pub plains_flatten: f32,
    pub warp_amp_plains: f32,
}

impl Default for WorldGenConfig {
    fn default() -> Self {
        Self {
            seed: 1337,
            enable_caves: false,

            base_height: 8,
            height_span: 48,
            height_freq: 0.007,

            warp_freq: 0.02,
            warp_amp: 25.0,

            caves_freq: 0.022,
            caves_thresh: 0.50,
            caves_tunnel_abs_pow: 0.85,

            cave_min_y: -120,
            cave_max_y: 56,

            caverns_freq: 0.009,
            caverns_thresh: 0.66,
            caverns_weight: 0.35,

            surface_shell: 4,
            entrance_depth: 14,
            entrance_bonus: 0.25,
            entrance_chance_freq: 0.018,
            entrance_chance_thresh: 0.72,
            entrance_open_depth: 3,
            entrance_open_radius: 1,

            caves_region_freq: 0.004,
            caves_region_thresh: 0.58,

            cave_y_scale: 0.75,

            plains_freq: 0.003,
            plains_threshold: 0.55,
            plains_blend: 0.15,
            plains_span: 8,
            plains_flatten: 0.6,
            warp_amp_plains: 6.0,
        }
    }
}

#[derive(Resource)]
pub struct CrosshairConfig {
    pub radius: f32, 
    pub thickness: f32,
    pub segments: usize,
    pub color: Color,
    pub visible_when_unlocked: bool,
}
impl Default for CrosshairConfig {
    fn default() -> Self {
        Self {
            radius: 8.0,
            thickness: 2.0,
            segments: 48,
            color: Color::WHITE,
            visible_when_unlocked: false,
        }
    }
}