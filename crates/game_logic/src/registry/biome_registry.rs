use bevy::app::{App, Plugin};
use bevy::prelude::*;
use game_core::configuration::WorldGenConfig;
use game_core::states::AppState;
use game_core::world::biome::BiomeRegistry;
use game_core::world::region::BiomeRegionAllocator;

pub struct BiomeInternalRegistry;

impl Plugin for BiomeInternalRegistry {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::Preload), start_biome_registry);
    }
}

fn start_biome_registry(
    mut commands: Commands,
    mut registry: ResMut<BiomeRegistry>,
    world_config: Res<WorldGenConfig>,
) {
    commands.insert_resource(BiomeRegionAllocator::with_seed(world_config.seed));
    let biomes = BiomeRegistry::load_all_from_assets_folder().unwrap();
    let count = biomes.len();
    for biome in biomes {
        registry.insert(biome);
    }
    debug!("Registered {} biomes successfully!", count);
    debug!("Biome names: {:?}", registry.iter().map(|(name, _)| name).collect::<Vec<&String>>());
}