pub mod blocks;

use bevy::prelude::*;

/// Size of a voxel in the world.
pub const VOXEL_SIZE: f32 = 1.0;

pub struct GameWorldModule;

impl Plugin for GameWorldModule {

    #[coverage(off)]
    fn build(&self, _app: &mut App) { }

}