#![feature(coverage_attribute)]

pub mod debug;
pub mod states;
pub mod configuration;
pub mod key_converter;
pub mod world;
pub mod player;
pub mod load_state;
pub mod events;
pub mod shader;

use crate::configuration::{CrosshairConfig, WorldGenConfig};
use crate::events::EventModule;
use crate::player::PlayerModule;
use crate::world::block::{MiningOverlayRoot, MiningState, SelectedBlock};
use bevy::prelude::*;

#[derive(Resource, Clone)]
pub struct BuildInfo {
    pub app_name: &'static str,
    pub app_version: &'static str,
    pub bevy_version: &'static str,
}

#[derive(Component)]
pub struct BlockCatalogPreviewCam;

#[derive(Resource, Default)]
pub struct BlockCatalogUiState { pub open: bool, pub root: Option<Entity> }

pub const UI_ACCENT_COLOR: Color = Color::srgb_u8(96, 94, 230);

pub struct GameCorePlugin;

impl Plugin for GameCorePlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.init_resource::<WorldGenConfig>();
        app.init_resource::<CrosshairConfig>();
        app.init_resource::<SelectedBlock>();
        app.init_resource::<MiningState>();
        app.init_resource::<MiningOverlayRoot>();
        app.add_plugins((PlayerModule, EventModule));
    }
}
