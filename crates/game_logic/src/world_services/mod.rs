mod save_service;

use crate::world_services::save_service::WorldSaveService;
use bevy::prelude::*;

pub struct WorldServices;

impl Plugin for WorldServices {
    fn build(&self, app: &mut App) {
        app.add_plugins(WorldSaveService);
    }
}