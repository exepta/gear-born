use bevy::input::mouse::MouseMotion;
use bevy::pbr::CascadeShadowConfigBuilder;
use bevy::prelude::*;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use bevy_rapier3d::prelude::*;
use game_core::configuration::GameConfig;
use game_core::key_converter::convert;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::BlockRegistry;


pub struct CameraPlugin;

impl Plugin for CameraPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 65.0,
            affects_lightmapped_meshes: false,
        })
            .add_systems(
                OnEnter(AppState::InGame(InGameStates::Game)),
                (spawn_scene, spawn_player),
            )
            .add_systems(
                Update,
                (
                    grab_cursor_on_click,
                    release_cursor_on_escape,
                    mouse_look,
                    player_move_kcc,
                )
                    .run_if(resource_exists::<BlockRegistry>),
            );
    }
}

#[derive(Component)]
struct Player;

#[derive(Component)]
struct PlayerCamera;

#[derive(Component)]
struct FpsController {
    yaw: f32,
    pitch: f32,
    speed: f32,
    sensitivity: f32,
}

#[derive(Component)]
struct DoubleTapSpace {
    last_press: f32,
}

#[derive(Component)]
struct FlightState { flying: bool }

#[derive(Component)]
struct PlayerKinematics {
    vel_y: f32,
}

fn spawn_scene(mut commands: Commands, block_registry: Res<BlockRegistry>) {
    commands.spawn((
        DirectionalLight {
            shadows_enabled: true,
            illuminance: 1_000.0,
            color: Color::WHITE,
            ..default()
        },
        CascadeShadowConfigBuilder {
            first_cascade_far_bound: 16.0,
            maximum_distance: 180.0,
            ..default()
        }
            .build(),
        Transform::from_xyz(4.0, 200.0, 4.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));

    info!("content: {:?}", block_registry.name_to_id);
}

fn spawn_player(mut commands: Commands, game_config: Res<GameConfig>) {
    let fog_color = Color::srgb(0.62, 0.72, 0.85);
    let fog_start = game_config.graphics.chunk_range as f32 * 25.0;
    let fog_end   = fog_start + 20.0;
    let clip_pad  = 0.25;
    let far_clip  = fog_end - clip_pad;

    let fov_deg: f32 = 90.0;

    let capsule_radius = 0.4;
    let capsule_half_height = 0.9;

    let player = commands
        .spawn((
            Player,
            Name::new("Player"),
            Transform::from_xyz(0.0, 400.0, 6.0),
            GlobalTransform::default(),

            RigidBody::KinematicPositionBased,
            Collider::capsule_y(capsule_half_height, capsule_radius),
            LockedAxes::ROTATION_LOCKED,
            KinematicCharacterController {
                offset: CharacterLength::Absolute(0.02),
                slide: true,
                autostep: Some(CharacterAutostep {
                    max_height: CharacterLength::Absolute(0.5),
                    min_width: CharacterLength::Absolute(0.2),
                    include_dynamic_bodies: false,
                }),
                snap_to_ground: Some(CharacterLength::Absolute(0.1)),
                ..default()
            },

            FpsController {
                yaw: 0.0,
                pitch: 0.0,
                speed: 6.0,
                sensitivity: 0.001,
            },
            PlayerKinematics { vel_y: 0.0 },
            FlightState { flying: false },
        ))
        .id();

    commands.entity(player).with_children(|c| {
        c.spawn((
            PlayerCamera,
            Camera3d::default(),
            Projection::Perspective(PerspectiveProjection {
                fov: fov_deg.to_radians(),
                near: 0.05,
                far:  far_clip.max(1.0),
                ..default()
            }),
            Camera {
                clear_color: ClearColorConfig::Custom(fog_color),
                ..default()
            },
            DistanceFog {
                color: fog_color,
                falloff: FogFalloff::Linear { start: fog_start, end: fog_end },
                ..default()
            },
            Transform::from_xyz(0.0, 1.6, 0.0),
            GlobalTransform::default(),
            Name::new("PlayerCamera"),
        ));
    });

    commands
        .entity(player)
        .insert(DoubleTapSpace { last_press: -1_000_000.0 });
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
    game_config: Res<GameConfig>,
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
    mut q_player: Query<(Entity, &mut Transform, &mut FpsController), (With<Player>, Without<PlayerCamera>)>,
    mut q_cam: Query<(&ChildOf, &mut Transform), (With<PlayerCamera>, Without<Player>)>,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    let Ok(window) = windows.single() else { return; };
    if window.cursor_options.grab_mode != CursorGrabMode::Locked {
        ev_motion.clear();
        return;
    }

    let Ok((player_entity,mut player_tf, mut ctrl)) = q_player.single_mut() else { return; };

    let mut delta = Vec2::ZERO;
    for ev in ev_motion.read() {
        delta += ev.delta;
    }
    if delta == Vec2::ZERO {
        return;
    }

    ctrl.yaw -= delta.x * ctrl.sensitivity;
    ctrl.pitch -= delta.y * ctrl.sensitivity;

    let limit = std::f32::consts::FRAC_PI_2 - 0.01;
    ctrl.pitch = ctrl.pitch.clamp(-limit, limit);

    player_tf.rotation = Quat::from_rotation_y(ctrl.yaw);

    for (parent, mut cam_tf) in q_cam.iter_mut() {
        if parent.parent() == player_entity {
            cam_tf.rotation = Quat::from_rotation_x(ctrl.pitch);
        }
    }
}

fn player_move_kcc(
    time: Res<Time>,
    keys: Res<ButtonInput<KeyCode>>,
    mut q_player: Query<(
        &Transform,
        &FpsController,
        &mut PlayerKinematics,
        &mut KinematicCharacterController,
        Option<&KinematicCharacterControllerOutput>,
        &mut FlightState,
        &mut DoubleTapSpace,
    ), With<Player>>,
    game_config: Res<GameConfig>,
) {
    let Ok((tf, ctrl, mut kin, mut kcc, kcc_out, mut flight, mut tap)) = q_player.single_mut() else { return; };

    // ---- Tuning ----
    let ground_speed     = ctrl.speed;
    let fly_multi        = 4.0;
    let fly_v_multi      = 4.0;
    let gravity          = 22.0;
    let fall_multi       = 2.2;
    let low_jump_multi   = 3.0;
    let jump_impulse     = 8.5;
    const DOUBLE_TAP_WIN: f32 = 0.28;

    let forward_key = convert(game_config.input.move_up.as_str()).expect("Invalid key");
    let back_key    = convert(game_config.input.move_down.as_str()).expect("Invalid key");
    let left_key    = convert(game_config.input.move_left.as_str()).expect("Invalid key");
    let right_key   = convert(game_config.input.move_right.as_str()).expect("Invalid key");

    let f = tf.forward();
    let r = tf.right();
    let forward = Vec3::new(f.x, 0.0, f.z).normalize_or_zero();
    let right   = Vec3::new(r.x, 0.0, r.z).normalize_or_zero();

    let mut wish = Vec3::ZERO;
    if keys.pressed(forward_key) { wish += forward; }
    if keys.pressed(back_key)    { wish -= forward; }
    if keys.pressed(left_key)    { wish -= right;   }
    if keys.pressed(right_key)   { wish += right;   }
    if wish.length_squared() > 0.0 { wish = wish.normalize(); }

    let dt        = time.delta_secs();
    let now       = time.elapsed_secs();
    let grounded  = kcc_out.map(|o| o.grounded).unwrap_or(false);

    if keys.just_pressed(KeyCode::Space) {
        if now - tap.last_press <= DOUBLE_TAP_WIN {
            // Double-tap: Toggle Fly
            flight.flying = !flight.flying;
            tap.last_press = -1_000_000.0;
            kin.vel_y = 0.0;
        } else {
            tap.last_press = now;
            if !flight.flying && grounded {
                kin.vel_y = jump_impulse;
            }
        }
    }

    kcc.snap_to_ground = if flight.flying { None } else { Some(CharacterLength::Absolute(0.2)) };

    let mut translation = Vec3::ZERO;

    if flight.flying {
        let mut up_down = 0.0;
        if keys.pressed(KeyCode::Space)     { up_down += 1.0; }
        if keys.pressed(KeyCode::ShiftLeft) { up_down -= 1.0; }

        translation += wish * ground_speed * fly_multi * dt;
        translation += Vec3::Y * up_down * ground_speed * fly_v_multi * dt;

        kin.vel_y = 0.0;
    } else {
        if grounded {

        } else {
            let mut g = gravity;
            if kin.vel_y < 0.0 {
                g *= fall_multi;
            } else if !keys.pressed(KeyCode::Space) {
                g *= low_jump_multi;
            }
            kin.vel_y -= g * dt;
        }

        translation += wish * ground_speed * dt;
        translation += Vec3::Y * kin.vel_y * dt;
    }

    kcc.translation = Some(translation);
}