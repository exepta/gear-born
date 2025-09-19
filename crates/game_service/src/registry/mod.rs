mod block_registry;

use bevy::prelude::*;
use crate::registry::block_registry::BlockInternalRegistry;

pub struct GameServiceRegistry;

impl Plugin for GameServiceRegistry {

    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(BlockInternalRegistry);
    }

}