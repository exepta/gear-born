use bevy::prelude::*;
use game_core::BlockCatalogPreviewCam;
use game_core::player::PlayerCamera;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{BlockRegistry, VOXEL_SIZE, fluid_at_world, get_block_world};
use game_core::world::chunk::ChunkMap;
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
    fog_camera: Option<Entity>,
    base_fog: Option<DistanceFog>,
}

#[derive(Component)]
struct UnderwaterOverlay;

// --- System -------------------------------------------------------

fn update_underwater_fx(
    mut commands: Commands,
    mut state: ResMut<UnderwaterFxState>,
    fluids: Option<Res<FluidMap>>,
    chunk_map: Option<Res<ChunkMap>>,
    blocks: Option<Res<BlockRegistry>>,
    mut q_player_cam: Query<
        (Entity, &GlobalTransform, Option<&mut DistanceFog>),
        With<PlayerCamera>,
    >,
    mut q_fallback_cam: Query<
        (Entity, &GlobalTransform, Option<&mut DistanceFog>),
        (With<Camera3d>, Without<PlayerCamera>, Without<BlockCatalogPreviewCam>),
    >,
    mut overlay_q: Query<&mut BackgroundColor, With<UnderwaterOverlay>>,
) {
    let cam = q_player_cam
        .iter_mut()
        .next()
        .or_else(|| q_fallback_cam.iter_mut().next());
    let Some((cam_entity, xf, fog_opt)) = cam else {
        return;
    };
    let eye = xf.translation();

    let wx = (eye.x / VOXEL_SIZE).floor() as i32;
    let wy = (eye.y / VOXEL_SIZE).floor() as i32;
    let wz = (eye.z / VOXEL_SIZE).floor() as i32;

    let underwater_fluid_map = fluids
        .as_ref()
        .is_some_and(|f| fluid_at_world(f, wx, wy, wz));
    let underwater_block_id = match (chunk_map.as_ref(), blocks.as_ref()) {
        (Some(cm), Some(reg)) => reg.is_fluid(get_block_world(cm, IVec3::new(wx, wy, wz))),
        _ => false,
    };
    let fallback_by_sea_height = eye.y <= ((SEA_LEVEL as f32 + 0.20) * VOXEL_SIZE);
    let needs_height_fallback = fluids.is_none() && (chunk_map.is_none() || blocks.is_none());
    let underwater = underwater_fluid_map
        || underwater_block_id
        || (needs_height_fallback && fallback_by_sea_height);

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
                    GlobalZIndex(9999),
                    UnderwaterOverlay,
                ))
                .id();
            state.overlay = Some(e);
        }
    } else if !underwater && state.was_underwater {
        if let Some(e) = state.overlay.take() {
            commands.entity(e).despawn();
        }
    }

    if state.fog_camera != Some(cam_entity) {
        state.fog_camera = Some(cam_entity);
        state.base_fog = fog_opt.as_ref().map(|fog| (**fog).clone());
    }

    if let Some(mut fog) = fog_opt {
        if state.base_fog.is_none() {
            state.base_fog = Some((*fog).clone());
        }

        if underwater {
            let depth = ((SEA_LEVEL as f32 * VOXEL_SIZE) - eye.y).max(0.0);
            let depth_factor = (depth / 8.0).clamp(0.0, 1.0);

            fog.color = Color::srgb(
                0.08 + 0.04 * depth_factor,
                0.26 + 0.08 * depth_factor,
                0.46 + 0.10 * depth_factor,
            );
            fog.falloff = FogFalloff::Linear {
                start: 0.1,
                end: 12.0 - 6.0 * depth_factor,
            };
        } else if let Some(base) = state.base_fog.clone() {
            *fog = base;
        }
    } else if underwater {
        commands.entity(cam_entity).insert(DistanceFog {
            color: Color::srgb(0.10, 0.30, 0.52),
            falloff: FogFalloff::Linear {
                start: 0.1,
                end: 9.5,
            },
            ..default()
        });
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

        if let Some(mut bg) = overlay_q.iter_mut().next() {
            let mut c = bg.0.to_linear();
            c.set_alpha(alpha);
            bg.0 = c.into();
        }
    }

    state.was_underwater = underwater;
}
