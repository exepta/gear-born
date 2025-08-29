#![coverage(off)]

use bevy::prelude::{Entity, GizmoConfigGroup, Reflect, Resource, Timer, TimerMode};
use sysinfo::System;

/// Represents the state of the World Inspector UI.
///
/// This resource holds a single boolean value indicating whether the World Inspector UI
/// is currently visible or hidden. The state can be toggled by user input (e.g., a key press),
/// and this struct is used to track the visibility of the World Inspector in the application.
///
/// The `WorldInspectorState` is initialized to `false` (hidden) by default.
///
/// # Fields
///
/// * `0`: A boolean value that represents the visibility of the World Inspector UI.
///   - `true`: The World Inspector is visible.
///   - `false`: The World Inspector is hidden.
#[derive(Resource, Default, Debug)]
pub struct WorldInspectorState(pub bool);

/// Gizmo configuration group for selection-related overlays.
///
/// Add this as a Bevy `Resource` to adjust gizmo settings (color, line width, etc.)
/// for selection visuals via Bevy’s `GizmoConfigStore`.
#[derive(Resource, Default, GizmoConfigGroup, Reflect)]
pub struct SelectionGizmoGroup;

/// Gizmo configuration group for rendering a chunk/grid visualization.
///
/// Use this marker resource to separately configure gizmos used to draw
/// a world/grid or chunk boundaries.
#[derive(Resource, Default, GizmoConfigGroup, Reflect)]
pub struct ChunkGridGizmos;

/// Runtime state for a simple on-screen debug overlay (e.g. FPS, system stats).
///
/// The overlay is created lazily: `root` and `text` are populated once the
/// corresponding UI entities are spawned.
#[derive(Resource, Default)]
pub struct DebugOverlayState {
    /// Whether the overlay should be visible.
    pub show: bool,
    /// Root UI entity of the overlay, if it has been created.
    pub root: Option<Entity>,
    /// Text node entity used to display the overlay contents, if created.
    pub text: Option<Entity>,
}

/// Controls rendering of a world-aligned debug grid.
///
/// `plane_y` specifies the Y elevation (in world units) of the grid plane.
#[derive(Resource, Default)]
pub struct DebugGridState {
    /// Whether the grid should be drawn.
    pub show: bool,
    /// World-space Y height of the grid plane.
    pub plane_y: f32,
}

/// Periodically sampled system/application performance metrics.
///
/// The underlying collector is `sysinfo::System` (`sys` field). Values are
/// updated on a repeating timer (`timer`) and are expected to be in:
/// - `cpu_percent`: global CPU usage in percent (0.0–100.0).
/// - `app_cpu_percent`: current process CPU usage in percent (0.0–100.0).
/// - `app_mem_bytes`: current process memory usage in **bytes**.
///
/// **Usage notes:**
/// - `System::new()` does not populate data; call `refresh_*` (e.g. `refresh_all`,
///   `refresh_cpu`, `refresh_processes`) before reading values.
/// - 'Timer' controls the sampling cadence; by default, it ticks every 0.5 s.
#[derive(Resource)]
pub struct SysStats {
    /// Sys_info handle used to query system and process metrics.
    pub sys: System,
    /// Global CPU utilization in percent.
    pub cpu_percent: f32,
    /// CPU utilization of the current application/process in percent.
    pub app_cpu_percent: f32,
    /// Memory usage of the current application/process in bytes.
    pub app_mem_bytes: u64,
    /// Repeating timer determining how often metrics are refreshed.
    pub timer: Timer,
}

impl Default for SysStats {
    /// Creates a `SysStats` with an empty `System` handle and a 0.5s sampling interval.
    ///
    /// After construction, call the appropriate `sys.refresh_*` methods on each
    /// timer tick before reading the metrics.
    fn default() -> Self {
        Self {
            sys: System::new(),
            cpu_percent: 0.0,
            app_cpu_percent: 0.0,
            app_mem_bytes: 0,
            timer: Timer::from_seconds(0.5, TimerMode::Repeating),
        }
    }
}
