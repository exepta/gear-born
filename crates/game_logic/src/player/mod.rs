mod look_at_service;
mod cross_hair;
mod initialize;
mod water_hud_fx;

use crate::player::cross_hair::CrosshairPlugin;
use crate::player::initialize::PlayerInitialize;
use crate::player::look_at_service::LookAtService;
use crate::player::water_hud_fx::UnderwaterFxPlugin;
use bevy::prelude::*;

pub struct PlayerServices;

impl Plugin for PlayerServices {
    fn build(&self, app: &mut App) {
        app.add_plugins((PlayerInitialize, LookAtService, CrosshairPlugin, UnderwaterFxPlugin));
    }
}