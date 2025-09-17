use bevy::prelude::*;
use game_core::states::{AppState, BeforeUiState};
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
) {
    let registry = BlockRegistry::load_all(&asset_server, &mut materials, "assets/blocks");
    commands.insert_resource(registry);
    next.set(AppState::Screen(BeforeUiState::Menu));
}
