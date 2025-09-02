use bevy::prelude::*;
use game_core::events::registry_events::{BlockRegistryEvent, RegistryState};
use game_core::states::{AppState, LoadingStates};
use game_core::world::block::BlockRegistry;

pub struct BlockInternalRegistry;

impl Plugin for BlockInternalRegistry {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::Preload), start_block_registry);
    }
}

fn start_block_registry(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut next: ResMut<NextState<AppState>>,
    mut reg_events: EventWriter<BlockRegistryEvent>
) {
    let registry = BlockRegistry::load_all(&asset_server, &mut materials, "assets/blocks");
    let count = registry.defs.len();
    commands.insert_resource(registry);
    
    reg_events.write(BlockRegistryEvent { state: RegistryState::Success, count });
    
    next.set(AppState::Loading(LoadingStates::BaseGen));
}
