use bevy::prelude::*;
use game_core::states::{AppState, LoadingStates};
use game_core::world::chunk::LoadCenter;

#[derive(Component)]
struct LoadingScreenRoot;

pub struct LoadingScreen;

impl Plugin for LoadingScreen {
    fn build(&self, app: &mut App) {
        app.add_systems(
            OnEnter(AppState::Loading(LoadingStates::BaseGen)),
            (enter_loading, spawn_loading_ui)
        )
            .add_systems(
                OnExit(AppState::Loading(LoadingStates::WaterGen)),
                despawn_loading_ui
            );
    }
}

fn enter_loading(mut commands: Commands) {
    commands.insert_resource(LoadCenter { world_xz: IVec2::new(0, 0) });
}

fn spawn_loading_ui(mut commands: Commands) {
    commands.spawn((
        Camera2d,
        Camera {
            clear_color: ClearColorConfig::Custom(Color::BLACK),
            ..default()
        },
        LoadingScreenRoot,
        Name::new("LoadingCamera"),
    ));

    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            ..default()
        },
        BackgroundColor(Color::NONE),
        LoadingScreenRoot,
        Name::new("LoadingRoot"),
    )).with_children(|p| {
        p.spawn((
            Text(String::from("Loading...")),
            TextColor(Color::WHITE),
            TextFont {
                font_size: 36.0,
                ..default()
            },
            Name::new("LoadingText"),
        ));
    });
}

fn despawn_loading_ui(mut commands: Commands, q: Query<Entity, With<LoadingScreenRoot>>) {
    for e in q.iter() { commands.entity(e).despawn(); }
}