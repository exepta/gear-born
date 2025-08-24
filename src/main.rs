#![feature(coverage_attribute)]

mod manager;

use crate::manager::ManagerPlugin;
use bevy::image::ImageSamplerDescriptor;
use bevy::log::{BoxedLayer, Level, LogPlugin};
use bevy::prelude::*;
use bevy::render::settings::{Backends, RenderCreation, WgpuFeatures, WgpuSettings};
use bevy::render::RenderPlugin;
use bevy::window::{WindowMode, WindowResolution};
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use chrono::Utc;
use dotenvy::dotenv;
use game_core::configuration::GameConfig;
use game_core::debug::WorldInspectorState;
use game_core::states::AppState;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tracing_subscriber::fmt::writer::BoxMakeWriter;
use tracing_subscriber::Layer;

/// Application entry point for debug builds.
/// Initializes logging, loads configuration, creates the Bevy app, and runs the core client.
///
/// This function is only included in debug builds.
#[cfg(debug_assertions)]
#[coverage(off)]
fn main() -> AppExit {
    load_log_env_filter();
    let options = GameConfig::new();
    let mut app = App::new();
    client_core(&mut app, options).run()
}

/// Application entry point for release builds.
/// Initializes configuration, creates the Bevy app, and runs the core client.
///
/// This function is only included in release builds.
#[cfg(not(debug_assertions))]
#[coverage(off)]
fn main() -> AppExit {
    let options = GameConfig::new();
    let mut app = App::new();
    client_core(&mut app, options).run()
}

/// Sets up the core Bevy application, inserting configuration and plugins.
///
/// # Parameters
/// - `app`: Mutable reference to the Bevy [`App`] instance.
/// - `config`: The loaded [`GameConfig`] containing window, graphics, input, and audio configuration.
///
/// # Returns
/// A mutable reference to the configured [`App`] instance.
///
/// # Behavior
/// - Inserts the [`GameConfig`] resource, making it available to all systems.
/// - Adds commonly used plugins: Egui, WorldInspector, and your [`ManagerPlugin`].
/// - Configures inspector state for debugging.
///
/// # Example
/// ```rust
/// let mut app = App::new();
/// let config = GameConfig::new();
/// client_core(&mut app, config).run();
/// ```
#[allow(dead_code)]
#[coverage(off)]
pub(crate) fn client_core(app: &mut App, config: GameConfig) -> &mut App {
    init_bevy_app(app, config.clone())
        .init_state::<AppState>()
        .insert_resource(config)
        .insert_resource(WorldInspectorState(false))
        .add_plugins(EguiPlugin::default())
        .add_plugins(WorldInspectorPlugin::default().run_if(check_world_inspector_state))
        .add_plugins(ManagerPlugin)
}

/// Initializes core Bevy app plugins and logging settings.
///
/// # Parameters
/// - `app`: A mutable reference to the [`App`] instance.
/// - `options`: [`ClientOptions`] containing window configuration.
///
/// # Returns
/// A mutable reference to the initialized [`App`] instance.
#[coverage(off)]
fn init_bevy_app(app: &mut App, config: GameConfig) -> &mut App {
    app.add_plugins(DefaultPlugins.set(
        WindowPlugin {
            primary_window: Some(Window {
                title: String::from("Bevy Client"),
                mode: if config.graphics.fullscreen { WindowMode::BorderlessFullscreen(MonitorSelection::Primary) } else { WindowMode::Windowed }, //BorderlessFullscreen(MonitorSelection::Primary)
                resolution: WindowResolution::new(config.graphics.window_width, config.graphics.window_height),
                ..default()
            }),
            ..default()
        }
    ).set(
        RenderPlugin {
            render_creation: RenderCreation::Automatic(create_gpu_settings(config.graphics.graphic_backend)),
            ..default()
        }
    ).set(ImagePlugin {
        default_sampler: ImageSamplerDescriptor::nearest(),
        ..default()
    }).set(LogPlugin {
        level: Level::DEBUG,
        filter: load_log_env_filter(),
        custom_layer: log_file_appender
    }))
        .insert_resource(ClearColor(Color::Srgba(Srgba::rgb_u8(20, 25,27))))
        .add_systems(Update, init_app_finish.run_if(in_state(AppState::AppInit).and(resource_exists::<GameConfig>)))
}

#[coverage(off)]
fn init_app_finish(mut next_state: ResMut<NextState<AppState>>) {
    info!("Finish initializing app...");
    next_state.set(AppState::Preload);
}

/// Creates GPU settings for rendering.
///
/// The settings include enabling Vulkan as the rendering backend and enabling
/// the `POLYGON_MODE_LINE` feature for wireframe rendering.
///
/// # Returns
/// A configured [`WgpuSettings`] instance.
///
/// # Example
/// ```rust
/// let gpu_settings = create_gpu_settings();
/// ```
fn create_gpu_settings(backend_str: String) -> WgpuSettings {
    let backend = match backend_str.as_str() {
        "auto" | "AUTO" | "primary" | "PRIMARY" => Some(Backends::PRIMARY),
        "vulkan" | "VULKAN" => Some(Backends::VULKAN),
        "dx12" | "DX12" => Some(Backends::DX12),
        "metal" | "METAL" => Some(Backends::METAL),
        _ => panic!("Invalid backend: {}", backend_str)
    };

    WgpuSettings {
        features: WgpuFeatures::POLYGON_MODE_LINE,
        backends: backend,
        ..default()
    }
}

/// Checks whether the World Inspector UI is currently enabled or not.
///
/// This function simply checks the state of the `WorldInspectorState`
/// and returns a boolean indicating whether the World Inspector UI is visible.
///
/// # Arguments
///
/// * `world_inspector_state`: A reference to the state of the world inspector UI.
///
/// # Returns
///
/// * `true` if the World Inspector UI is visible (enabled).
/// * `false` if the World Inspector UI is not visible (disabled).
#[coverage(off)]
fn check_world_inspector_state(
    world_inspector_state: Res<WorldInspectorState>,
) -> bool {
    world_inspector_state.0
}

/// Initializes a log file appender for the application.
///
/// This function creates a `logs` directory if it does not exist and generates a log file
/// with a timestamped name in the format `bevy-DD-MM-YYYY.log`. It then sets up a
/// logging layer that writes log messages to this file.
///
/// # Parameters
/// - `_app`: A mutable reference to the Bevy `App`. (Currently unused)
///
/// # Returns
/// - `Some(BoxedLayer)`: If the log file was successfully created and opened.
/// - `None`: If there was an error creating the log directory or opening the file.
///
/// # Logging Details
/// - The log file is set up to append new logs.
/// - ANSI formatting is disabled for better readability in plain text files.
/// - The log writer ensures proper synchronization using `Arc<Mutex<File>>`.
fn log_file_appender(_app: &mut App) -> Option<BoxedLayer> {
    let log_dir = PathBuf::from("logs");
    if let Err(e) = std::fs::create_dir_all(&log_dir) {
        eprintln!("Failed to create log directory: {}", e);
        return None;
    }

    let timestamp = Utc::now().format("bevy-%d-%m-%Y.log").to_string();
    let log_path = log_dir.join(timestamp);

    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_path)
        .ok()?;

    let file_arc = Arc::new(Mutex::new(file));

    let _shutdown_logger = StartLogText {
        file: Arc::clone(&file_arc),
    };

    let writer = BoxMakeWriter::new(move || {
        let file = file_arc.lock().unwrap().try_clone().expect("Failed to clone log file handle");
        Box::new(file) as Box<dyn Write + Send>
    });

    Some(Box::new(tracing_subscriber::fmt::layer()
        .with_ansi(false)
        .with_writer(writer)
        .boxed()
    ))
}

/// Loads the `LOG_ENV_FILTER` environment variable from a `.env` file and returns it as a `String`.
///
/// This function uses the `dotenv` crate to load environment variables from a `.env` file in the project root (if present).
/// If the `LOG_ENV_FILTER` variable is not set, it defaults to `"error"`.
///
/// # Returns
/// A `String` containing the log filter settings to use for logging frameworks (e.g., tracing).
///
/// # Example
/// ```rust
/// let log_filter = load_log_env_filter();
/// // Pass `log_filter` to your logging/tracing setup
/// ```
///
#[coverage(off)]
fn load_log_env_filter() -> String {
    dotenv().ok();
    let env = env::var("LOG_ENV_FILTER").unwrap_or_else(|_| "error".to_string());
    env.to_string()
}

/// Helper struct to insert a start log entry when logging is initialized.
///
/// When this struct is dropped, it writes a separator message to the log file
/// to indicate when logging has started.
struct StartLogText {
    file: Arc<Mutex<File>>, // Shared reference to the log file
}

impl Drop for StartLogText {

    #[coverage(off)]
    fn drop(&mut self) {
        let mut file = self.file.lock().unwrap();
        let _ = writeln!(
            file,
            "\n====================================== [ Start ] ======================================\n"
        );
        let _ = file.flush();
    }
}

// =================================================================================================
//
//                                            Unit Tests
//
// =================================================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    #[test]
    #[serial]
    fn test_load_log_env_filter_from_env() {
        unsafe { env::set_var("LOG_ENV_FILTER", "my_test_value"); }
        let value = load_log_env_filter();
        assert_eq!(value, "my_test_value");
        unsafe { env::remove_var("LOG_ENV_FILTER"); }
    }

    #[test]
    fn test_create_gpu_settings_primary() {
        let settings = create_gpu_settings("primary".to_string());
        assert_eq!(settings.backends, Some(Backends::PRIMARY));
        assert_eq!(settings.features, WgpuFeatures::POLYGON_MODE_LINE);

        let settings = create_gpu_settings("AUTO".to_string());
        assert_eq!(settings.backends, Some(Backends::PRIMARY));
    }

    #[test]
    fn test_create_gpu_settings_vulkan() {
        let settings = create_gpu_settings("vulkan".to_string());
        assert_eq!(settings.backends, Some(Backends::VULKAN));
    }

    #[test]
    fn test_create_gpu_settings_dx12() {
        let settings = create_gpu_settings("DX12".to_string());
        assert_eq!(settings.backends, Some(Backends::DX12));
    }

    #[test]
    fn test_create_gpu_settings_metal() {
        let settings = create_gpu_settings("metal".to_string());
        assert_eq!(settings.backends, Some(Backends::METAL));
    }

    #[test]
    #[should_panic(expected = "Invalid backend")]
    fn test_create_gpu_settings_invalid() {
        let _ = create_gpu_settings("unknown-backend".to_string());
    }
}
