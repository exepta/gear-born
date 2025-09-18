#![feature(coverage_attribute)]

use bevy::prelude::*;
use game_core::camera::UiCamera;
use game_core::states::AppState;

pub struct GameUiPlugin;

impl Plugin for GameUiPlugin {

    #[coverage(off)]
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::Preload), create_ui_camera);
    }

}

/// Create the [`UiCamera`] entity for us. This Camera is used by Ui components
/// to show. Warning [`WorldInspectorUI`] can have problems with this type of
/// camera. Make sure you used for gizmos correct groups!
fn create_ui_camera(mut commands: Commands) {
    commands.spawn((
        Name::new("UiCamera"),
        Camera2d::default(),
        Camera {
            order: 2,
            ..default()
        },
        UiCamera
    ));
    debug!("Create UiCamera successfully!");
}