use bevy::prelude::*;
use bevy_rapier3d::prelude::*;
use game_core::configuration::GameConfig;
use game_core::debug::WorldInspectorState;
use game_core::key_converter::convert;
use game_core::GameCorePlugin;
use game_logic::GameLogicPlugin;
use game_world::GameWorldPlugin;

pub struct ManagerPlugin;

impl Plugin for ManagerPlugin {
    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_plugins(RapierPhysicsPlugin::<NoUserData>::default());
        app.add_plugins(RapierDebugRenderPlugin {
            enabled: false,
            ..default()
        });

        app.add_plugins((
            GameCorePlugin,
            GameWorldPlugin,
            GameLogicPlugin,
        ));

        app.add_systems(Update, toggle_world_inspector);
    }
}

#[coverage(off)]
fn toggle_world_inspector(
    mut debug_context: ResMut<WorldInspectorState>,
    keyboard: ResMut<ButtonInput<KeyCode>>,
    game_config: Res<GameConfig>
) {
    let key = convert(game_config.input.world_inspector.as_str())
        .expect("Invalid key for world inspector");
    if keyboard.just_pressed(key) {
        debug_context.0 = !debug_context.0;
    }
}