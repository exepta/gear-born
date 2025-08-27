#![feature(coverage_attribute)]

mod camera;
mod registry;

use crate::camera::CameraPlugin;
use bevy::prelude::*;
use crate::registry::block_registry::BlockInternalRegistry;

pub struct GameLogicPlugin;

impl Plugin for GameLogicPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins((BlockInternalRegistry, CameraPlugin));
    }
}
