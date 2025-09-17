use bevy::prelude::*;
use bevy::ecs::schedule::common_conditions::resource_changed;
use game_core::configuration::{GameConfig, TextureResolution};
use game_core::states::{AppState, BeforeUiState};
use game_core::world::block::BlockRegistry;
/* ===========================================================
   Tracks which texture resolution is currently active
   =========================================================== */
#[derive(Resource, Clone, Copy, Debug, Default, Eq, PartialEq)]
struct LoadedTextureRes(pub TextureResolution);

/* ===========================================================
   Plugin wiring
   =========================================================== */
pub struct BlockInternalRegistry;

impl Plugin for BlockInternalRegistry {
    fn build(&self, app: &mut App) {
        app.init_resource::<LoadedTextureRes>()
            // initial load during Preload
            .add_systems(OnEnter(AppState::Preload), start_block_registry)
            // hot reload when GameConfig changes (any time during Update)
            .add_systems(
                Update,
                reload_registry_if_texture_res_changed
                    .run_if(resource_changed::<GameConfig>),
            );
    }
}

/* ===========================================================
   Initial load (kept as-is, but we also remember the active res)
   =========================================================== */
fn start_block_registry(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut next: ResMut<NextState<AppState>>,
    game_config: Res<GameConfig>,
    mut loaded_res: ResMut<LoadedTextureRes>,
) {
    let res = game_config.graphics.texture_res;
    debug!("Loaded texture res: {:?}", res);
    let registry = BlockRegistry::load_all_with_res(
        &asset_server,
        &mut materials,
        "assets/blocks",
        res,
    );

    *loaded_res = LoadedTextureRes(res);
    commands.insert_resource(registry);
    next.set(AppState::Screen(BeforeUiState::Menu));
}

/* ===========================================================
   Hot reload when GameConfig.texture_res changes
   - Rebuild BlockRegistry with new resolution
   - Reassign materials on existing block entities by name
   =========================================================== */
fn reload_registry_if_texture_res_changed(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    game_config: Res<GameConfig>,
    mut loaded_res: ResMut<LoadedTextureRes>,

    // any entity spawned via `spawn_block_by_id` has a Name equal to the block name
    mut blocks: Query<(&Name, &mut MeshMaterial3d<StandardMaterial>)>,
) {
    let new_res = game_config.graphics.texture_res;

    // Skip if resolution didn't actually change.
    if loaded_res.0 == new_res {
        return;
    }

    // Rebuild the registry for the new resolution.
    let new_registry = BlockRegistry::load_all_with_res(
        &asset_server,
        &mut materials,
        "assets/blocks",
        new_res,
    );

    // Update existing entities' materials by matching their Name to a block id.
    // This assumes block entities were spawned with `Name::new(reg.name(id))`.
    for (name, mut mat) in &mut blocks {
        if let Some(id) = new_registry.id_opt(name.as_str()) {
            // Replace material handle with the one from the new registry.
            *mat = MeshMaterial3d(new_registry.material(id));
        }
    }

    // Publish the new registry and remember the active resolution.
    commands.insert_resource(new_registry);
    *loaded_res = LoadedTextureRes(new_res);
}
