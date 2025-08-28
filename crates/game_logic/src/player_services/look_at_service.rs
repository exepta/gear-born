use bevy::prelude::*;
use game_core::player::selection::{BlockHit, SelectionState};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{id_any, BlockId, BlockRegistry, Face};
use game_core::world::chunk::{ChunkData, ChunkMap, SubchunkDirty, VoxelStage};
use game_core::world::chunk_dim::*;

pub struct LookAtService;

impl Plugin for LookAtService {
    fn build(&self, app: &mut App) {
        app.configure_sets(Update, (
            VoxelStage::Input,
            VoxelStage::WorldEdit,
            VoxelStage::Meshing,
        ).chain());

        app.add_systems(
            Update,
            (
                update_selection.in_set(VoxelStage::Input),
                draw_selection_gizmo.in_set(VoxelStage::Input),
                handle_break_and_place.in_set(VoxelStage::WorldEdit),
            ).chain().run_if(in_state(AppState::InGame(InGameStates::Game)))
        );
    }
}

fn update_selection(
    mut sel: ResMut<SelectionState>,
    q_cam: Query<(&GlobalTransform, &Camera), With<Camera3d>>,
    chunk_map: Res<ChunkMap>,
) {
    let (tf, _cam) = if let Ok(v) = q_cam.single() { v } else { return; };
    let origin = tf.translation();
    let dir = tf.forward();

    let max_dist = 8.0;

    sel.hit = ray_cast_voxels(origin, *dir, max_dist, &chunk_map);
}

fn draw_selection_gizmo(
    sel: Res<SelectionState>,
    mut gizmos: Gizmos,
) {
    if let Some(hit) = sel.hit {
        let center = Vec3::new(
            hit.block_pos.x as f32 + 0.5,
            hit.block_pos.y as f32 + 0.5,
            hit.block_pos.z as f32 + 0.5,
        );
        let size = Vec3::splat(1.002);
        gizmos.cuboid(Transform::from_translation(center).with_scale(size), Color::BLACK);
    }
}

fn handle_break_and_place(
    buttons: Res<ButtonInput<MouseButton>>,
    mut chunk_map: ResMut<ChunkMap>,
    reg: Res<BlockRegistry>,
    sel: Res<SelectionState>,
    mut ev_dirty: EventWriter<SubchunkDirty>,
) {
    let Some(hit) = sel.hit else { return; };

    if buttons.just_pressed(MouseButton::Left) {
        if let Some(mut access) = world_access_mut(&mut chunk_map, hit.block_pos) {
            access.set(0);
        }
        mark_dirty_block_and_neighbors(&mut chunk_map, hit.block_pos, &mut ev_dirty);
    }

    if buttons.just_pressed(MouseButton::Right) {
        let stone = id_any(&reg, &["stone_block","stone"]);
        if stone != 0 {
            if let Some(mut access) = world_access_mut(&mut chunk_map, hit.place_pos) {
                if access.get() == 0 { access.set(stone); }
            }
            mark_dirty_block_and_neighbors(&mut chunk_map, hit.place_pos, &mut ev_dirty);
        }
    }
}

fn ray_cast_voxels(origin: Vec3, dir_in: Vec3, max_dist: f32, chunk_map: &ChunkMap) -> Option<BlockHit> {
    let dir = dir_in.normalize_or_zero();
    if dir == Vec3::ZERO { return None; }

    let mut x = origin.x.floor() as i32;
    let mut y = origin.y.floor() as i32;
    let mut z = origin.z.floor() as i32;

    let step_x = if dir.x > 0.0 { 1 } else if dir.x < 0.0 { -1 } else { 0 };
    let step_y = if dir.y > 0.0 { 1 } else if dir.y < 0.0 { -1 } else { 0 };
    let step_z = if dir.z > 0.0 { 1 } else if dir.z < 0.0 { -1 } else { 0 };

    let next_boundary = |p: f32, step: i32| -> f32 {
        if step > 0 { p.floor() + 1.0 } else { p.ceil() - 1.0 }
    };

    let mut t_max_x = if step_x != 0 { (next_boundary(origin.x, step_x) - origin.x) / dir.x } else { f32::INFINITY };
    let mut t_max_y = if step_y != 0 { (next_boundary(origin.y, step_y) - origin.y) / dir.y } else { f32::INFINITY };
    let mut t_max_z = if step_z != 0 { (next_boundary(origin.z, step_z) - origin.z) / dir.z } else { f32::INFINITY };

    let t_delta_x = if step_x != 0 { 1.0 / dir.x.abs() } else { f32::INFINITY };
    let t_delta_y = if step_y != 0 { 1.0 / dir.y.abs() } else { f32::INFINITY };
    let t_delta_z = if step_z != 0 { 1.0 / dir.z.abs() } else { f32::INFINITY };

    let mut last_empty = IVec3::new(x, y, z);
    let mut t: f32;

    for _ in 0..512 {
        let id = get_block_world(chunk_map, IVec3::new(x, y, z));
        if id != 0 {
            let face = if t_max_x < t_max_y && t_max_x < t_max_z {
                if step_x > 0 { Face::West } else { Face::East }
            } else if t_max_y < t_max_z {
                if step_y > 0 { Face::Bottom } else { Face::Top }
            } else {
                if step_z > 0 { Face::North } else { Face::South }
            };

            return Some(BlockHit {
                block_pos: IVec3::new(x, y, z),
                face,
                place_pos: last_empty,
            });
        }

        last_empty = IVec3::new(x, y, z);

        if t_max_x < t_max_y && t_max_x < t_max_z {
            x += step_x;
            t = t_max_x;
            t_max_x += t_delta_x;
        } else if t_max_y < t_max_z {
            y += step_y;
            t = t_max_y;
            t_max_y += t_delta_y;
        } else {
            z += step_z;
            t = t_max_z;
            t_max_z += t_delta_z;
        }

        if t > max_dist { break; }
        if y < Y_MIN || y > Y_MAX { break; }
    }

    None
}

#[inline]
fn get_block_world(chunk_map: &ChunkMap, wp: IVec3) -> BlockId {
    if wp.y < Y_MIN || wp.y > Y_MAX { return 0; }
    let (cc, local) = world_to_chunk_xz(wp.x, wp.z);
    let Some(chunk) = chunk_map.chunks.get(&cc) else { return 0; };
    let lx = local.x as usize;
    let lz = local.y as usize;
    let ly = world_y_to_local(wp.y);
    chunk.get(lx, ly, lz)
}

fn world_access_mut(chunk_map: &'_ mut ChunkMap, wp: IVec3)
                    -> Option<WorldMutAccess<'_>>
{
    if wp.y < Y_MIN || wp.y > Y_MAX { return None; }
    let (cc, local) = world_to_chunk_xz(wp.x, wp.z);
    let chunk = chunk_map.chunks.get_mut(&cc)?;
    let lx = local.x as usize;
    let lz = local.y as usize;
    let ly = world_y_to_local(wp.y);
    if lx >= CX || lz >= CZ || ly >= CY { return None; }
    let sub = ly / SEC_H;
    Some(WorldMutAccess { chunk, lx, ly, lz, sub })
}

pub struct WorldMutAccess<'a> {
    chunk: &'a mut ChunkData,
    lx: usize, ly: usize, lz: usize,
    sub: usize,
}
impl<'a> WorldMutAccess<'a> {
    #[inline] pub fn get(&self) -> BlockId { self.chunk.get(self.lx, self.ly, self.lz) }
    #[inline] pub fn set(&mut self, id: BlockId) {
        self.chunk.set(self.lx, self.ly, self.lz, id);
        self.chunk.mark_dirty_local_y(self.sub);
    }
}

// world_y <-> local_y
#[inline] fn world_y_to_local(wy: i32) -> usize { (wy - Y_MIN) as usize }

fn mark_dirty_block_and_neighbors(
    chunk_map: &mut ChunkMap,
    wp: IVec3,
    ev: &mut EventWriter<SubchunkDirty>,
) {
    const OFFS: [IVec3; 7] = [
        IVec3::new(0, 0, 0),
        IVec3::new( 1, 0,  0),
        IVec3::new(-1, 0,  0),
        IVec3::new( 0, 1,  0),
        IVec3::new( 0,-1,  0),
        IVec3::new( 0, 0,  1),
        IVec3::new( 0, 0, -1),
    ];

    for o in OFFS {
        let p = wp + o;
        if p.y < Y_MIN || p.y > Y_MAX { continue; }

        // immer die Mapper aus chunk_dim benutzen
        let (coord, local_xz) = world_to_chunk_xz(p.x, p.z);
        let ly = world_y_to_local(p.y);
        let sub = ly / SEC_H;

        let mut mark = |c: IVec2, s: usize| {
            if s < SEC_COUNT {
                if let Some(ch) = chunk_map.chunks.get_mut(&c) {
                    ch.mark_dirty_local_y(s);
                    ev.write(SubchunkDirty { coord: c, sub: s }); // <-- Event
                }
            }
        };

        mark(coord, sub);
        if ly % SEC_H == 0 && sub > 0            { mark(coord, sub - 1); }
        if ly % SEC_H == SEC_H - 1 && sub + 1 < SEC_COUNT { mark(coord, sub + 1); }

        let _ = local_xz;
    }
}