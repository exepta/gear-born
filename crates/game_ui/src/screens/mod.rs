mod loading_screen;

use crate::screens::loading_screen::LoadingScreen;
use bevy::prelude::*;

pub struct ScreenManager;

impl Plugin for ScreenManager {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(LoadingScreen);
    }
}