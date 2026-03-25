mod html_screen;
mod loading_screen;
mod catalog_hud_screen;

use crate::screens::catalog_hud_screen::BlockCatalogUiPlugin;
use crate::screens::html_screen::HtmlScreenPlugin;
use crate::screens::loading_screen::LoadingScreen;
use bevy::prelude::*;

pub struct ScreenManager;

impl Plugin for ScreenManager {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((HtmlScreenPlugin, LoadingScreen, BlockCatalogUiPlugin));
    }
}
