use bevy::prelude::*;
use bevy::ecs::schedule::common_conditions::resource_changed;
use game_core::config::{GlobalConfig, TextureQuality};
use game_core::states::{AppState, UiState};
use game_core::world::blocks::registry::BlockRegistry;
/* ===========================================================
   Tracks which texture resolution is currently active
   =========================================================== */
#[derive(Resource, Clone, Copy, Debug, Default, Eq, PartialEq)]
struct LoadedTextureQuality(pub TextureQuality);

/* ===========================================================
   Plugin wiring
   =========================================================== */
pub struct BlockInternalRegistry;

impl Plugin for BlockInternalRegistry {
    fn build(&self, app: &mut App) {
        app.init_resource::<LoadedTextureQuality>()
            // initial load during Preload
            .add_systems(OnEnter(AppState::Preload), start_block_registry)
            // hot reload when GameConfig changes (any time during Update)
            .add_systems(
                Update,
                reload_registry_if_texture_res_changed
                    .run_if(resource_changed::<GlobalConfig>),
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
    global_config: Res<GlobalConfig>,
    mut loaded_res: ResMut<LoadedTextureQuality>,
) {
    let res = global_config.graphics_config.texture_quality;
    debug!("Loaded texture res: {:?}", res);
    let registry = BlockRegistry::load_all_with_resources(
        &asset_server,
        &mut materials,
        "assets/blocks",
        res,
    );

    *loaded_res = LoadedTextureQuality(res);
    commands.insert_resource(registry.clone());

    debug!("Registered: ({} blocks)",registry.blocks.len());
    next.set(AppState::Screen(UiState::Menu));
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
    global_config: Res<GlobalConfig>,
    mut loaded_res: ResMut<LoadedTextureQuality>,

    // any entity spawned via `spawn_block_by_id` has a Name equal to the block name
    mut blocks: Query<(&Name, &mut MeshMaterial3d<StandardMaterial>)>,
) {
    let new_res = global_config.graphics_config.texture_quality;

    // Skip if the resolution didn't change.
    if loaded_res.0 == new_res {
        return;
    }

    // Rebuild the registry for the new resolution.
    let new_registry = BlockRegistry::load_all_with_resources(
        &asset_server,
        &mut materials,
        "assets/blocks",
        new_res,
    );

    // Update existing entities' materials by matching their Name to a block id.
    // This assumes block entities were spawned with `Name::new(reg.name(id))`.
    for (name, mut mat) in &mut blocks {
        if let Some(id) = new_registry.id_optional(name.as_str()) {
            // Replace a material handle with the one from the new registry.
            *mat = MeshMaterial3d(new_registry.material(id));
        }
    }

    // Publish the new registry and remember the active resolution.
    commands.insert_resource(new_registry);
    *loaded_res = LoadedTextureQuality(new_res);
    debug!("Reloaded block registry with new resolution: {:?}", new_res);
}
