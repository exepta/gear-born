mod chunk;
mod save_service;

use bevy::prelude::*;
use crate::world_services::chunk::ChunkHandlerService;
use crate::world_services::save_service::WorldSaveService;

pub struct WorldServices;

impl Plugin for WorldServices {
    fn build(&self, app: &mut App) {
        app.add_plugins((ChunkHandlerService, WorldSaveService));
    }
}