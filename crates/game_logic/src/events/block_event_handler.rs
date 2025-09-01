use bevy::ecs::relationship::RelatedSpawnerCommands;
use bevy::pbr::{NotShadowCaster, NotShadowReceiver};
use bevy::prelude::*;
use bevy::render::view::{NoFrustumCulling, RenderLayers};
use game_core::events::chunk_events::SubChunkNeedRemeshEvent;
use game_core::events::player_block_events::{BlockBreakByPlayerEvent, BlockPlaceByPlayerEvent};
use game_core::player::selection::SelectionState;
use game_core::player::{GameMode, GameModeState};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::*;
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use game_core::world::fluid::FluidMap;
use game_core::world::{mark_dirty_block_and_neighbors, world_access_mut};

#[derive(Component)]
struct MiningOverlay;

#[derive(Component)]
struct MiningOverlayFace;

#[derive(Clone, Copy)]
enum Axis { XY, XZ, YZ }

pub struct BlockEventHandler;

impl Plugin for BlockEventHandler {
    fn build(&self, app: &mut App) {
        app
            .add_systems(
            Update,
            (
                (block_break_handler, sync_mining_overlay).chain(),
                block_place_handler,
            ).run_if(in_state(AppState::InGame(InGameStates::Game)))
        );
    }
}

fn block_break_handler(
    time: Res<Time>,
    buttons: Res<ButtonInput<MouseButton>>,
    selection: Res<SelectionState>,
    registry: Res<BlockRegistry>,
    game_mode: Res<GameModeState>,

    mut state: ResMut<MiningState>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubChunkNeedRemeshEvent>,
    mut break_ev: EventWriter<BlockBreakByPlayerEvent>,
) {
    // --- Creative: instant break on click ---
    if matches!(game_mode.0, GameMode::Creative) {
        if !buttons.just_pressed(MouseButton::Left) {
            state.target = None;
            return;
        }

        let Some(hit) = selection.hit else {
            state.target = None;
            return;
        };

        let id_now = get_block_world(&chunk_map, hit.block_pos);
        if id_now == 0 {
            state.target = None;
            return;
        }

        // remove block immediately
        if let Some(mut access) = world_access_mut(&mut chunk_map, hit.block_pos) {
            access.set(0);
        }
        mark_dirty_block_and_neighbors(&mut chunk_map, hit.block_pos, &mut ev_dirty);

        let (chunk_coord, l) = world_to_chunk_xz(hit.block_pos.x, hit.block_pos.z);
        let lx = l.x as u8;
        let lz = l.y as u8;
        let ly = (hit.block_pos.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

        break_ev.write(BlockBreakByPlayerEvent {
            chunk_coord,
            location: hit.block_pos,
            chunk_x: lx,
            chunk_y: ly as u16,
            chunk_z: lz,
            block_id: id_now,
            block_name: registry.name_opt(id_now).unwrap_or("").to_string(),
        });

        state.target = None;
        return; // done for creative
    }

    // --- Survival: timed mining as before ---
    if !buttons.pressed(MouseButton::Left) {
        state.target = None;
        return;
    }

    let Some(hit) = selection.hit else {
        state.target = None;
        return;
    };

    let id_now = get_block_world(&chunk_map, hit.block_pos);
    if id_now == 0 {
        state.target = None;
        return;
    }

    let now = time.elapsed_secs();

    let restart = match state.target {
        None => true,
        Some(target) => target.loc != hit.block_pos || target.id != id_now,
    };

    if restart {
        state.target = Some(MiningTarget {
            loc: hit.block_pos,
            id: id_now,
            started_at: now,
            duration: break_time_for(id_now, &registry),
        });
    }

    let target = state.target.as_ref().unwrap();
    let progress = (now - target.started_at) / target.duration;

    if progress < 1.0 { return; }

    let world_loc = target.loc;
    if let Some(mut access) = world_access_mut(&mut  chunk_map, world_loc) {
        access.set(0);
    }
    mark_dirty_block_and_neighbors(&mut chunk_map, world_loc, &mut ev_dirty);

    let (chunk_coord, l) = world_to_chunk_xz(world_loc.x, world_loc.z);
    let lx = l.x as u8;
    let lz = l.y as u8;
    let ly = (world_loc.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

    break_ev.write(BlockBreakByPlayerEvent {
        chunk_coord,
        location: world_loc,
        chunk_x: lx,
        chunk_y: ly as u16,
        chunk_z: lz,
        block_id: target.id,
        block_name: registry.name_opt(target.id).unwrap_or("").to_string(),
    });

    state.target = None;
}

fn block_place_handler(
    buttons: Res<ButtonInput<MouseButton>>,
    sel: Res<SelectionState>,
    selected: Res<SelectedBlock>,
    registry: Res<BlockRegistry>,

    mut fluids: ResMut<FluidMap>,
    mut chunk_map: ResMut<ChunkMap>,
    mut ev_dirty: EventWriter<SubChunkNeedRemeshEvent>,
    mut place_ev: EventWriter<BlockPlaceByPlayerEvent>,
) {
    if !buttons.just_pressed(MouseButton::Right) { return; }
    let id = selected.id;
    if id == 0 { return; }
    let Some(hit) = sel.hit else { return; };

    let world_pos = hit.place_pos;
    let (chunk_coord, l) = world_to_chunk_xz(world_pos.x, world_pos.z);
    let lx = l.x.clamp(0, (CX as i32 - 1) as u32) as usize;
    let lz = l.y.clamp(0, (CZ as i32 - 1) as u32) as usize;
    let ly = (world_pos.y - Y_MIN).clamp(0, CY as i32 - 1) as usize;

    let can_place = chunk_map
        .chunks
        .get(&chunk_coord)
        .map(|ch| ch.get(lx, ly, lz) == 0)
        .unwrap_or(false);
    if !can_place { return; }

    if let Some(fc) = fluids.0.get_mut(&chunk_coord) {
        fc.set(lx, ly, lz, false);
    }

    if let Some(mut access) = world_access_mut(&mut chunk_map, world_pos) {
        access.set(id);
    }

    mark_dirty_block_and_neighbors(&mut chunk_map, world_pos, &mut ev_dirty);

    let name = registry.name_opt(id).unwrap_or("").to_string();
    place_ev.write(BlockPlaceByPlayerEvent {
        location: world_pos,
        block_id: id,
        block_name: name,
    });
}

fn sync_mining_overlay(
    mut commands: Commands,
    mut root: ResMut<MiningOverlayRoot>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut mats: ResMut<Assets<StandardMaterial>>,
    time: Res<Time>,
    state: Res<MiningState>,
    mut q_faces: Query<(&mut Transform, &MiningOverlayFace), (With<MiningOverlayFace>, Without<MiningOverlay>)>,
    mut q_parent_tf: Query<&mut Transform, (With<MiningOverlay>, Without<MiningOverlayFace>)>,
) {
    let Some(target) = state.target else {
        if let Some(e) = root.0.take() { commands.entity(e).despawn(); }
        return;
    };

    let now = time.elapsed_secs();
    let progress = ((now - target.started_at) / target.duration).clamp(0.0, 1.0);

    let s = VOXEL_SIZE;
    let center = Vec3::new(
        (target.loc.x as f32 + 0.5) * s,
        (target.loc.y as f32 + 0.5) * s,
        (target.loc.z as f32 + 0.5) * s,
    );

    let parent_e = if let Some(e) = root.0 {
        e
    } else {
        let e = spawn_overlay_at(&mut commands, &mut meshes, &mut mats, center, Some(RenderLayers::layer(2)), progress);
        root.0 = Some(e);
        e
    };

    if let Ok(mut tf) = q_parent_tf.get_mut(parent_e) {
        tf.translation = center;
    }

    let max_scale = 0.98 * s;
    let size = max_scale * progress;
    let face_scale = Vec3::new(size, size, 1.0);

    for (mut tf, _) in q_faces.iter_mut() {
        tf.scale = face_scale;
    }

    if progress >= 1.0 {
        if let Some(e) = root.0.take() {
            commands.entity(e).despawn();
        }
    }
}

#[inline]
fn spawn_overlay_at(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    mats: &mut Assets<StandardMaterial>,
    world_center: Vec3,
    layer: Option<RenderLayers>,
    initial_progress: f32,
) -> Entity {
    let quad = meshes.add(unit_centered_quad());
    let mat  = mats.add(StandardMaterial {
        base_color: Color::srgba(0.9, 0.9, 0.9, 0.02),
        alpha_mode: AlphaMode::Blend,
        unlit: true,
        cull_mode: None,
        perceptual_roughness: 1.0,
        ..default()
    });

    let s   = VOXEL_SIZE;
    let half= 0.5 * s;
    let eps = 0.003 * s;

    let max_scale   = 0.98 * s;
    let init_scale  = (initial_progress.clamp(0.0, 1.0).max(0.001)) * max_scale;
    let init_vec    = Vec3::new(init_scale, init_scale, 1.0);

    let mut parent = commands.spawn((
        MiningOverlay,
        Visibility::default(),
        NoFrustumCulling,
        Transform::from_translation(world_center),
        GlobalTransform::default(),
        NotShadowCaster,
        NotShadowReceiver,
        Name::new("MiningOverlay"),
    ));
    if let Some(l) = layer.as_ref() { parent.insert(l.clone()); }
    let parent_id = parent.id();

    let spawn_face = |c: &mut RelatedSpawnerCommands<ChildOf>, _: Axis, tf: Transform| {
        let mut e = c.spawn((
            MiningOverlayFace,
            Visibility::default(),
            Mesh3d(quad.clone()),
            MeshMaterial3d(mat.clone()),
            tf.with_scale(init_vec),
            GlobalTransform::default(),
            NotShadowCaster, NotShadowReceiver,
            Name::new("MiningOverlayFace"),
        ));
        if let Some(l) = layer.as_ref() { e.insert(l.clone()); }
    };

    commands.entity(parent_id).with_children(|c| {
        // +Z / -Z (XY)
        spawn_face(c, Axis::XY, Transform::from_translation(Vec3::new(0.0, 0.0,  half + eps)));
        spawn_face(c, Axis::XY, Transform::from_rotation(Quat::from_rotation_y(std::f32::consts::PI))
            .with_translation(Vec3::new(0.0, 0.0, -half - eps)));

        // +Y / -Y (XZ)
        spawn_face(c, Axis::XZ, Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2))
            .with_translation(Vec3::new(0.0,  half + eps, 0.0)));
        spawn_face(c, Axis::XZ, Transform::from_rotation(Quat::from_rotation_x(std::f32::consts::FRAC_PI_2))
            .with_translation(Vec3::new(0.0, -half - eps, 0.0)));

        // +X / -X (YZ)
        spawn_face(c, Axis::YZ, Transform::from_rotation(Quat::from_rotation_y(-std::f32::consts::FRAC_PI_2))
            .with_translation(Vec3::new( half + eps, 0.0, 0.0)));
        spawn_face(c, Axis::YZ, Transform::from_rotation(Quat::from_rotation_y(std::f32::consts::FRAC_PI_2))
            .with_translation(Vec3::new(-half - eps, 0.0, 0.0)));
    });

    parent_id
}

#[inline]
fn unit_centered_quad() -> Mesh {
    use bevy::render::mesh::{Indices, Mesh, PrimitiveTopology};
    let mut m = Mesh::new(PrimitiveTopology::TriangleList, Default::default());
    m.insert_attribute(Mesh::ATTRIBUTE_POSITION, vec![
        [-0.5, -0.5, 0.0],
        [ 0.5, -0.5, 0.0],
        [ 0.5,  0.5, 0.0],
        [-0.5,  0.5, 0.0],
    ]);
    m.insert_attribute(Mesh::ATTRIBUTE_NORMAL, vec![[0.0, 0.0, 1.0]; 4]);
    m.insert_attribute(Mesh::ATTRIBUTE_UV_0, vec![[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0]]);
    m.insert_indices(Indices::U32(vec![0,1,2, 0,2,3]));
    m
}