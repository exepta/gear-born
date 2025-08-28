mod look_at_service;
mod cross_hair;

use crate::player_services::cross_hair::CrosshairPlugin;
use crate::player_services::look_at_service::LookAtService;
use bevy::prelude::*;

pub struct PlayerServices;

impl Plugin for PlayerServices {
    fn build(&self, app: &mut App) {
        app.add_plugins((LookAtService, CrosshairPlugin));
    }
}