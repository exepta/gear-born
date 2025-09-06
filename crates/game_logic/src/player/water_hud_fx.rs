use bevy::pbr::FogVolume;
use bevy::prelude::*;
use game_core::player::PlayerCamera;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{fluid_at_world, VOXEL_SIZE};
use game_core::world::chunk::SEA_LEVEL;
use game_core::world::fluid::FluidMap;

pub struct UnderwaterFxPlugin;

impl Plugin for UnderwaterFxPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<UnderwaterFxState>()
            .add_systems(Update, update_underwater_fx.run_if(in_state(AppState::InGame(InGameStates::Game))));
    }
}

// --- State & Marker -----------------------------------------------

#[derive(Resource, Default)]
struct UnderwaterFxState {
    overlay: Option<Entity>,
    was_underwater: bool,
}

#[derive(Component)]
struct UnderwaterOverlay;

// --- System -------------------------------------------------------

fn update_underwater_fx(
    mut commands: Commands,
    mut state: ResMut<UnderwaterFxState>,
    fluids: Res<FluidMap>,
    cam_q: Query<(Entity, &GlobalTransform), With<PlayerCamera>>,
    mut overlay_q: Query<&mut BackgroundColor, With<UnderwaterOverlay>>,
) {
    let Ok((entity,xf)) = cam_q.single() else { return; };
    let eye = xf.translation();

    let wx = (eye.x / VOXEL_SIZE).floor() as i32;
    let wy = (eye.y / VOXEL_SIZE).floor() as i32;
    let wz = (eye.z / VOXEL_SIZE).floor() as i32;

    let underwater = fluid_at_world(&fluids, wx, wy, wz);

    // --- Enter / Leave -------------------------------------------
    if underwater && !state.was_underwater {
        info!("Entering underwater");
        if state.overlay.is_none() {
            let e = commands
                .spawn((
                    Node {
                        position_type: PositionType::Absolute,
                        left: Val::Px(0.0), right: Val::Px(0.0),
                        top: Val::Px(0.0), bottom: Val::Px(0.0),
                        ..default()
                    },
                    BackgroundColor(Color::srgba(0.12, 0.35, 0.7, 0.35)),
                    ZIndex(1000),
                    UnderwaterOverlay,
                ))
                .id();
            state.overlay = Some(e);

            commands.entity(entity).insert(FogVolume { fog_color: Color::srgba(0.12, 0.35, 0.7, 0.35), density_factor: 0.5, ..default() });
        }
    } else if !underwater && state.was_underwater {
        if let Some(e) = state.overlay.take() {
            commands.entity(e).despawn();
        }

        commands.entity(entity).remove::<FogVolume>();
    }

    if underwater {
        let view_dir = xf.forward();
        let up = Vec3::Y;

        let t = view_dir.dot(up).clamp(0.0, 1.0);
        
        let depth = (SEA_LEVEL as f32 - eye.y).max(0.0);
        let depth_factor = (depth / 6.0).clamp(0.0, 1.0);

        let base = 0.45;
        let extra_look = 0.30 * t;
        let extra_depth = 0.25 * depth_factor;
        let alpha = (base + extra_look + extra_depth).clamp(0.0, 0.85);

        if let Ok(mut bg) = overlay_q.single_mut() {
            let mut c = bg.0.to_linear();
            c.set_alpha(alpha);
            bg.0 = c.into();
        }
    }

    state.was_underwater = underwater;
}