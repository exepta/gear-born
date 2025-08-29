#![feature(coverage_attribute)]

mod screens;

use crate::screens::ScreenManager;
use bevy::prelude::*;

pub struct GameUiPlugin;

impl Plugin for GameUiPlugin {

    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(ScreenManager);
    }
}
