pub mod selection;

use bevy::prelude::*;
use crate::player::selection::SelectionState;

pub struct PlayerModule;

impl Plugin for PlayerModule {
    fn build(&self, app: &mut App) {
        app.init_resource::<SelectionState>();
    }
}