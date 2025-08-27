mod look_at_service;

use bevy::prelude::*;
use crate::player_services::look_at_service::LookAtService;

pub struct PlayerServices;

impl Plugin for PlayerServices {
    fn build(&self, app: &mut App) {
        app.add_plugins(LookAtService);
    }
}