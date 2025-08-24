use bevy::prelude::*;
use bevy_rapier3d::prelude::*;
use game_core::GameCorePlugin;

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
        ));
    }
}