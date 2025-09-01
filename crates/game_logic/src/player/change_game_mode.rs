use bevy::prelude::*;
use game_core::configuration::GameConfig;
use game_core::key_converter::convert;
use game_core::player::{FlightState, GameMode, GameModeState};
use game_core::states::{AppState, InGameStates};

pub struct ChangeGameModeHandler;


impl Plugin for ChangeGameModeHandler {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, change_mode.run_if(in_state(AppState::InGame(InGameStates::Game))));
    }
}

fn change_mode(
    mut game_mode: ResMut<GameModeState>,
    keys: Res<ButtonInput<KeyCode>>,
    game_config: Res<GameConfig>,
    mut fly_state: Query<&mut FlightState>,
) {
    let key = convert(game_config.input.toggle_game_mode.as_str()).expect("Invalid key");
    if keys.just_pressed(key) {
        game_mode.0 = match game_mode.0 {
            GameMode::Survival => GameMode::Creative,
            GameMode::Creative => GameMode::Survival,
        };

        let mut fly_state = fly_state.single_mut().unwrap();
        fly_state.flying = game_mode.0 == GameMode::Creative;
    }
}