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

#[derive(Resource, Default, GizmoConfigGroup, Reflect)]
pub struct SelectionGizmoGroup;

#[derive(Resource, Default, GizmoConfigGroup, Reflect)]
pub struct ChunkGridGizmos;

#[derive(Resource, Default)]
pub struct DebugOverlayState {
    pub show: bool,
    pub root: Option<Entity>,
    pub text: Option<Entity>,
}

#[derive(Resource, Default)]
pub struct DebugGridState {
    pub show: bool,
    pub plane_y: f32,
}

#[derive(Resource)]
pub struct SysStats {
    pub sys: System,
    pub cpu_percent: f32, 
    pub app_cpu_percent: f32,
    pub app_mem_bytes: u64,
    pub timer: Timer,
}

impl Default for SysStats {
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