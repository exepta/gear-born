pub mod selection;

use crate::player::selection::SelectionState;
use bevy::prelude::*;

pub struct PlayerModule;

impl Plugin for PlayerModule {
    fn build(&self, app: &mut App) {
        app.init_resource::<SelectionState>();
    }
}

/// Marker component for the **player** entity.
///
/// Typically attached to the entity that owns input, physics, and/or camera parenting.
/// Useful for queries (e.g., `Query<Entity, With<Player>>`).
#[derive(Component)]
pub struct Player;

/// Marker component for the **first-person camera** associated with the player.
///
/// Usually attached to a `Camera` entity that is parented to `Player` or follows it.
#[derive(Component)]
pub struct PlayerCamera;

/// Simple first-person movement and look controller state.
///
/// - `yaw`: rotation around the world **+Y** axis (left/right).
/// - `pitch`: rotation around the local **+X** axis (up/down).
/// - `speed`: movement speed in world units **per second**.
/// - `sensitivity`: input multiplier converting mouse/controller deltas
///   to angular change (commonly radians per input unit).
#[derive(Component)]
pub struct FpsController {
    /// Yaw angle (left/right). Conventionally in **radians**.
    pub yaw: f32,
    /// Pitch angle (up/down). Conventionally in **radians**.
    pub pitch: f32,
    /// Linear movement speed in world units per second.
    pub speed: f32,
    /// Look sensitivity multiplier (applied to input deltas).
    pub sensitivity: f32,
}

/// Flight / noclip toggle for the player.
///
/// When `flying == true`, typical behavior is to disable gravity and collisions
/// and allow free 3D movement using the FPS controls.
#[derive(Component)]
pub struct FlightState { pub flying: bool }