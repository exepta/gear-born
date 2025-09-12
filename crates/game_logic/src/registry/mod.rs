mod block_registry;
mod biome_registry;

use bevy::prelude::*;
use crate::registry::biome_registry::BiomeInternalRegistry;
use crate::registry::block_registry::BlockInternalRegistry;

pub struct RegistryPlugin;

impl Plugin for RegistryPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((BlockInternalRegistry, BiomeInternalRegistry));
    }
}