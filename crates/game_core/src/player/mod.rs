pub mod selection;

use crate::player::selection::SelectionState;
use bevy::prelude::*;

pub struct PlayerModule;

impl Plugin for PlayerModule {
    fn build(&self, app: &mut App) {
        app.init_resource::<SelectionState>();
    }
}

#[derive(Component)]
pub struct Player;

#[derive(Component)]
pub struct PlayerCamera;

#[derive(Component)]
pub struct FpsController {
    pub yaw: f32,
    pub pitch: f32,
    pub speed: f32,
    pub sensitivity: f32,
}

#[derive(Component)]
pub struct FlightState { pub flying: bool }