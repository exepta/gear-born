use bevy::prelude::*;
use bevy_extended_ui::html::{HtmlEvent, HtmlSource};
use bevy_extended_ui::io::HtmlAsset;
use bevy_extended_ui::registry::UiRegistry;
use bevy_extended_ui::{ExtendedCam, ExtendedUiConfiguration, ExtendedUiPlugin};
use bevy_extended_ui_macros::html_fn;
use game_core::BlockCatalogUiState;
use game_core::states::{AppState, InGameStates, LoadingStates};

pub struct HtmlScreenPlugin;

const GAME_UI_ID: &str = "gear-born-ui";
const LOADING_UI_ID: &str = "gear-born-loading-ui";

impl Plugin for HtmlScreenPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(ExtendedUiPlugin)
            .init_resource::<BlockCatalogUiState>()
            .add_systems(Startup, (configure_extended_ui, load_html_ui))
            .add_systems(Update, sync_extended_ui_for_state);
    }
}

fn configure_extended_ui(mut config: ResMut<ExtendedUiConfiguration>) {
    config.camera = ExtendedCam::Default;
}

fn load_html_ui(mut reg: ResMut<UiRegistry>, asset_server: Res<AssetServer>) {
    let game_handle: Handle<HtmlAsset> = asset_server.load("ui/game_ui.html");
    let loading_handle: Handle<HtmlAsset> = asset_server.load("ui/loading_ui.html");

    reg.add(GAME_UI_ID.to_string(), HtmlSource::from_handle(game_handle));
    reg.add(LOADING_UI_ID.to_string(), HtmlSource::from_handle(loading_handle));
}

fn sync_extended_ui_for_state(mut reg: ResMut<UiRegistry>, app_state: Res<State<AppState>>) {
    let target = match app_state.get() {
        AppState::InGame(InGameStates::Game) => GAME_UI_ID,
        AppState::Loading(LoadingStates::BaseGen)
        | AppState::Loading(LoadingStates::WaterGen)
        | AppState::Loading(LoadingStates::CaveGen) => LOADING_UI_ID,
        _ => LOADING_UI_ID,
    };

    let active = reg.current.as_ref().and_then(|v| v.first()).map(String::as_str);
    if active != Some(target) {
        reg.use_ui(target);
    }
}

#[html_fn("toggle_inventory")]
fn toggle_inventory(In(_event): In<HtmlEvent>, mut ui_state: ResMut<BlockCatalogUiState>) {
    ui_state.open = !ui_state.open;
}

#[html_fn("close_inventory")]
fn close_inventory(In(_event): In<HtmlEvent>, mut ui_state: ResMut<BlockCatalogUiState>) {
    ui_state.open = false;
}
