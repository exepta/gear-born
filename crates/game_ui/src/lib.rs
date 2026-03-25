#![feature(coverage_attribute)]

mod default_font;
mod screens;

use crate::default_font::DefaultFontHandler;
use crate::screens::ScreenManager;
use bevy::prelude::*;

pub struct GameUiPlugin;

impl Plugin for GameUiPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((DefaultFontHandler, ScreenManager));
    }
}
