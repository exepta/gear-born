use bevy::input::mouse::MouseMotion;
use bevy::prelude::*;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use game_core::configuration::GameConfig;
use game_core::key_converter::convert;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{BlockRegistry};

pub struct CameraPlugin;

impl Plugin for CameraPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 150.0,
            affects_lightmapped_meshes: false,
        })
            .add_systems(OnEnter(AppState::InGame(InGameStates::Game)), (spawn_scene, spawn_camera))
            .add_systems(
                Update,
                (grab_cursor_on_click, release_cursor_on_escape, mouse_look, creative_movement).run_if(resource_exists::<BlockRegistry>),
            );
    }
}

#[derive(Component)]
struct FpsCamera {
    yaw: f32,
    pitch: f32,
    speed: f32,       // m/s
    sensitivity: f32, // rad per pixel
}

fn spawn_scene(
    mut commands: Commands,
    block_registry: Res<BlockRegistry>,
) {

/*    block::spawn_block_by_name(
        &mut commands,
        &mut meshes,
        &block_registry,
        Blocks::Stone,
        Vec3::ZERO,
        1.0,
    );*/

    commands.spawn((
        DirectionalLight::default(),
        Transform::from_xyz(4.0, 8.0, 4.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));

    info!("content: {:?}", block_registry.name_to_id);
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 18.0, 6.0).looking_at(Vec3::new(0.0, 2.0, 0.0), Vec3::Y),
        FpsCamera {
            yaw: 0.0,
            pitch: 0.0,
            speed: 20.0,
            sensitivity: 0.001,
        },
    ));
}

fn grab_cursor_on_click(
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    mouse: Res<ButtonInput<MouseButton>>,
) {
    if !mouse.just_pressed(MouseButton::Left) {
        return;
    }
    if let Ok(mut win) = windows.single_mut() {
        win.cursor_options.grab_mode = CursorGrabMode::Locked;
        win.cursor_options.visible = false;
    }
}

fn release_cursor_on_escape(
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    keys: Res<ButtonInput<KeyCode>>,
    game_config: Res<GameConfig>
) {
    let unlock = convert(game_config.input.mouse_screen_unlock.as_str())
        .expect("Invalid mouse screen unlock");

    if !keys.just_pressed(unlock) {
        return;
    }
    if let Ok(mut win) = windows.single_mut() {
        win.cursor_options.grab_mode = CursorGrabMode::None;
        win.cursor_options.visible = true;
    }
}

fn mouse_look(
    mut ev_motion: EventReader<MouseMotion>,
    mut q_cam: Query<(&mut Transform, &mut FpsCamera)>,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    let Ok(window) = windows.single() else { return; };
    if window.cursor_options.grab_mode != CursorGrabMode::Locked {
        ev_motion.clear();
        return;
    }

    let Ok((mut transform, mut cam)) = q_cam.single_mut() else { return; };

    let mut delta = Vec2::ZERO;
    for ev in ev_motion.read() {
        delta += ev.delta;
    }
    if delta == Vec2::ZERO {
        return;
    }

    cam.yaw -= delta.x * cam.sensitivity;
    cam.pitch -= delta.y * cam.sensitivity;

    let limit = std::f32::consts::FRAC_PI_2 - 0.01;
    cam.pitch = cam.pitch.clamp(-limit, limit);

    transform.rotation = Quat::from_euler(EulerRot::YXZ, cam.yaw, cam.pitch, 0.0);
}

fn creative_movement(
    time: Res<Time>,
    mut q_cam: Query<(&mut Transform, &FpsCamera)>,
    keys: Res<ButtonInput<KeyCode>>,
    game_config: Res<GameConfig>
) {
    let Ok((mut transform, cam)) = q_cam.single_mut() else { return; };
    let mut wish_dir = Vec3::ZERO;

    let forward = transform.forward();
    let right = transform.right();

    let forward_key = convert(game_config.input.move_up.as_str())
        .expect("Invalid key for forward movement");
    let back_key = convert(game_config.input.move_down.as_str())
        .expect("Invalid key for backward movement");
    let left_key = convert(game_config.input.move_left.as_str())
        .expect("Invalid key for left movement");
    let right_key = convert(game_config.input.move_right.as_str())
        .expect("Invalid key for right movement");

    if keys.pressed(forward_key) {
        wish_dir += *forward;
    }
    if keys.pressed(back_key) {
        wish_dir -= *forward;
    }
    if keys.pressed(left_key) {
        wish_dir -= *right;
    }
    if keys.pressed(right_key) {
        wish_dir += *right;
    }

    if keys.pressed(KeyCode::Space) {
        wish_dir += Vec3::Y;
    }
    if keys.pressed(KeyCode::ShiftLeft) {
        wish_dir -= Vec3::Y;
    }

    if wish_dir.length_squared() > 0.0 {
        wish_dir = wish_dir.normalize();
        transform.translation += wish_dir * cam.speed * time.delta_secs();
    }
}