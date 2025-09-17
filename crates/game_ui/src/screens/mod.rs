mod loading_screen;
mod catalog_hud_screen;
mod menu_screen;

use crate::screens::catalog_hud_screen::BlockCatalogUiPlugin;
use crate::screens::loading_screen::LoadingScreen;
use bevy::prelude::*;
use crate::screens::menu_screen::MenuUiPlugin;

pub struct ScreenManager;

impl Plugin for ScreenManager {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((MenuUiPlugin, LoadingScreen, BlockCatalogUiPlugin));
    }
}