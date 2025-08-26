#![feature(coverage_attribute)]

mod camera;

use crate::camera::CameraPlugin;
use bevy::prelude::*;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(CameraPlugin);
    }
}
