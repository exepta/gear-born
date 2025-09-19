#![feature(coverage_attribute)]

mod registry;

use bevy::prelude::*;
use crate::registry::GameServiceRegistry;

pub struct GameServicePlugin;

impl Plugin for GameServicePlugin {

    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(GameServiceRegistry);
    }

}