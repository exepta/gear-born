mod look_at_service;
mod cross_hair;
mod initialize;
mod water_hud_fx;

use crate::player_services::cross_hair::CrosshairPlugin;
use crate::player_services::initialize::PlayerInitialize;
use crate::player_services::look_at_service::LookAtService;
use crate::player_services::water_hud_fx::UnderwaterFxPlugin;
use bevy::prelude::*;

pub struct PlayerServices;

impl Plugin for PlayerServices {
    fn build(&self, app: &mut App) {
        app.add_plugins((PlayerInitialize, LookAtService, CrosshairPlugin, UnderwaterFxPlugin));
    }
}