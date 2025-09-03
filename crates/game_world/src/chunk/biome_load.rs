use bevy::asset::LoadedFolder;
use bevy::prelude::*;
use game_core::world::biome::{BiomeAsset, BiomeRegistry};

#[derive(Resource, Default)]
struct BiomeFolderState {
    folder: Option<Handle<LoadedFolder>>,
    populated: bool,
}

pub struct BiomeLoadHandler;

impl Plugin for BiomeLoadHandler {
    fn build(&self, app: &mut App) {
        app.init_asset::<BiomeAsset>()
            .init_resource::<BiomeRegistry>()
            .init_resource::<BiomeFolderState>();

        app.add_systems(Startup, queue_biome_folder_load)
            .add_systems(Update, (populate_registry_once_loaded, sync_registry_on_hot_reload));
    }
}

fn queue_biome_folder_load(
    mut state: ResMut<BiomeFolderState>,
    asset_server: Res<AssetServer>,
) {
    let handle = asset_server.load_folder("biomes");
    state.folder = Some(handle);
}

fn populate_registry_once_loaded(
    mut state: ResMut<BiomeFolderState>,
    asset_server: Res<AssetServer>,
    folders: Res<Assets<LoadedFolder>>,
    biomes: Res<Assets<BiomeAsset>>,
    mut registry: ResMut<BiomeRegistry>,
) {
    let Some(folder_handle) = state.folder.as_ref() else { return; };

    if !asset_server.is_loaded_with_dependencies(folder_handle) {
        return;
    }

    let Some(folder) = folders.get(folder_handle) else { return; };

    for untyped in folder.handles.iter() {
        let handle: Handle<BiomeAsset> = untyped.clone().typed();
        if let Some(biome) = biomes.get(&handle) {
            registry.insert(biome.name.clone(), handle.clone());
        }
    }

    if !state.populated {
        state.populated = true;
        info!(
            "Biome registry populated ({} entries)",
            registry.len()
        );
    }
}

fn sync_registry_on_hot_reload(
    mut events: EventReader<AssetEvent<BiomeAsset>>,
    biomes: Res<Assets<BiomeAsset>>,
    mut registry: ResMut<BiomeRegistry>,
) {
    use bevy::asset::AssetEvent;

    for ev in events.read() {
        match ev {
            AssetEvent::LoadedWithDependencies { id } | AssetEvent::Modified { id } => {
                let handle = Handle::Weak(*id);
                if let Some(biome) = biomes.get(*id) {
                    registry.insert(biome.name.clone(), handle.clone());
                }
            }
            AssetEvent::Removed { .. } => {

            }
            _ => {}
        }
    }
}