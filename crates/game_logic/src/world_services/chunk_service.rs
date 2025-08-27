use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology, VertexAttributeValues};
use bevy::tasks::futures_lite::future;
use bevy::tasks::{AsyncComputeTaskPool, Task};
use fastnoise_lite::{FastNoiseLite, FractalType, NoiseType};
use game_core::configuration::WorldGenConfig;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{id_any, BlockId, BlockRegistry, Face, UvRect};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use std::collections::{HashMap, VecDeque};

#[derive(Resource, Default)]
struct MeshBacklog(VecDeque<(IVec2, usize)>);

#[derive(Resource, Default)]
struct ChunkMeshIndex {
    pub map: HashMap<(IVec2, u8, BlockId), Entity>,
}

/// Pending Chunk-Generate-Tasks
#[derive(Resource, Default)]
struct PendingGen(HashMap<IVec2, Task<(IVec2, ChunkData)>>);

/// Pending Mesh-Tasks pro (coord, sub)
#[derive(Resource, Default)]
struct PendingMesh(HashMap<(IVec2, usize), Task<((IVec2, usize), Vec<(BlockId, MeshBuild)>)>>);

#[derive(Clone, Copy)]
struct RegLiteEntry {
    top: UvRect, bottom: UvRect, north: UvRect, east: UvRect, south: UvRect, west: UvRect,
    opaque: bool,
}
#[derive(Clone)]
struct RegLite {
    map: HashMap<BlockId, RegLiteEntry>,
}

impl RegLite {
    fn from_reg(reg: &BlockRegistry) -> Self {
        let mut map = HashMap::new();
        for &id in reg.name_to_id.values() {
            if id == 0 { continue; }
            map.insert(id, RegLiteEntry {
                top: reg.uv(id, Face::Top),
                bottom: reg.uv(id, Face::Bottom),
                north: reg.uv(id, Face::North),
                east: reg.uv(id, Face::East),
                south: reg.uv(id, Face::South),
                west: reg.uv(id, Face::West),
                opaque: reg.def(id).stats.opaque,
            });
        }
        Self { map }
    }
    #[inline] fn uv(&self, id: BlockId, face: Face) -> UvRect {
        let e = self.map.get(&id).expect("unknown id");
        match face {
            Face::Top => e.top, Face::Bottom => e.bottom, Face::North => e.north,
            Face::East => e.east, Face::South => e.south, Face::West => e.west,
        }
    }
    #[inline] fn opaque(&self, id: BlockId) -> bool {
        self.map.get(&id).map(|e| e.opaque).unwrap_or(false)
    }
}

struct MeshBuild {
    pos: Vec<[f32;3]>,
    nrm: Vec<[f32;3]>,
    uv:  Vec<[f32;2]>,
    idx: Vec<u32>,
}
impl MeshBuild {
    fn new() -> Self { Self { pos:vec![], nrm:vec![], uv:vec![], idx:vec![] } }
    fn quad(&mut self, q:[[f32;3];4], n:[f32;3], uv:[[f32;2];4]) {
        let base = self.pos.len() as u32;
        self.pos.extend_from_slice(&q);
        self.nrm.extend_from_slice(&[n;4]);
        self.uv.extend_from_slice(&uv);
        self.idx.extend_from_slice(&[base,base+1,base+2, base,base+2,base+3]);
    }
    fn into_mesh(self) -> Mesh {
        let mut m = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, self.pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   self.nrm);
        m.insert_attribute(Mesh::ATTRIBUTE_UV_0,     self.uv);

        // <= 65k Vertices? -> U16-Indices
        if self.idx.len() <= u16::MAX as usize {
            let idx_u16: Vec<u16> = self.idx.into_iter().map(|i| i as u16).collect();
            m.insert_indices(Indices::U16(idx_u16));
        } else {
            m.insert_indices(Indices::U32(self.idx));
        }

        m
    }
    #[allow(dead_code)]
    fn mesh_is_empty(m: &Mesh) -> bool {
        match m.attribute(Mesh::ATTRIBUTE_POSITION) {
            Some(VertexAttributeValues::Float32x3(v)) => v.is_empty(),
            Some(VertexAttributeValues::Float32(v)) => v.is_empty(),
            _ => true,
        }
    }
}

#[derive(Clone)]
struct BorderSnapshot {
    y0: usize,
    y1: usize,
    east:  Option<Vec<BlockId>>,
    west:  Option<Vec<BlockId>>,
    south: Option<Vec<BlockId>>,
    north: Option<Vec<BlockId>>,
}

pub struct ChunkService;

const LOAD_RADIUS: i32 = 8;
const KEEP_RADIUS: i32 = LOAD_RADIUS + 1;

const MAX_INFLIGHT_MESH: usize = 64;
const MAX_INFLIGHT_GEN:  usize = 32;

impl Plugin for ChunkService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<ChunkMeshIndex>()
            .init_resource::<MeshBacklog>()
            .init_resource::<PendingGen>()
            .init_resource::<PendingMesh>()
            .add_systems(Update, (
                schedule_chunk_generation,
                collect_generated_chunks,
                collect_meshed_subchunks,
                schedule_remesh_tasks_from_events.in_set(VoxelStage::Meshing),
                drain_mesh_backlog,
                unload_far_chunks,
            ).chain()
                .run_if(in_state(AppState::InGame(InGameStates::Game))));
    }
}

fn schedule_chunk_generation(
    mut pending: ResMut<PendingGen>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    gen_cfg: Res<WorldGenConfig>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    if !can_spawn_gen(&pending) { return; }

    let ids = (
        id_any(&reg, &["grass_block","grass"]),
        id_any(&reg, &["dirt_block","dirt"]),
        id_any(&reg, &["stone_block","stone"]),
    );
    let cfg_clone = gen_cfg.clone();

    let mut budget = MAX_INFLIGHT_GEN.saturating_sub(pending.0.len()).min(8);

    for dz in -LOAD_RADIUS..=LOAD_RADIUS {
        for dx in -LOAD_RADIUS..=LOAD_RADIUS {
            if budget == 0 { return; }
            let c = IVec2::new(center_c.x + dx, center_c.y + dz);
            if chunk_map.chunks.contains_key(&c) || pending.0.contains_key(&c) { continue; }

            let pool = AsyncComputeTaskPool::get();
            let ids_copy = ids;
            let cfg = cfg_clone.clone();
            let task = pool.spawn(async move {
                let data = generate_chunk_async_noise(c, ids_copy, cfg).await;
                (c, data)
            });
            pending.0.insert(c, task);
            budget -= 1;
        }
    }
}

fn drain_mesh_backlog(
    mut backlog: ResMut<MeshBacklog>,
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    if chunk_map.chunks.is_empty() { backlog.0.clear(); return; }

    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    while can_spawn_mesh(&pending_mesh) {
        let Some((coord, sub)) = backlog.0.pop_front() else { break; };
        if pending_mesh.0.contains_key(&(coord, sub)) { continue; }
        let Some(chunk) = chunk_map.chunks.get(&coord) else { continue; };

        let chunk_copy = chunk.clone();
        let reg_copy   = reg_lite.clone();
        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = snapshot_borders(&chunk_map, coord, y0, y1);

        let key = (coord, sub);
        let t = pool.spawn(async move {
            let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
            (key, builds)
        });
        pending_mesh.0.insert(key, t);
    }
}

fn collect_generated_chunks(
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    mut chunk_map: ResMut<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    let reg_lite = RegLite::from_reg(&reg);
    let mut finished = Vec::new();

    for (coord, task) in pending_gen.0.iter_mut() {
        if let Some((c, data)) = future::block_on(future::poll_once(task)) {
            chunk_map.chunks.insert(c, data.clone());

            // Subchunks dieses Chunks
            let pool = AsyncComputeTaskPool::get();
            for sub in 0..SEC_COUNT {
                let key = (c, sub);
                let y0 = sub * SEC_H;
                let y1 = (y0 + SEC_H).min(CY);
                let borders = snapshot_borders(&chunk_map, c, y0, y1);

                if can_spawn_mesh(&pending_mesh) {
                    let chunk_copy = data.clone();
                    let reg_copy = reg_lite.clone();
                    let t = pool.spawn(async move {
                        let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                        ((c, sub), builds)
                    });
                    pending_mesh.0.insert(key, t);
                } else {
                    enqueue_mesh(&mut backlog, &pending_mesh, key);
                }
            }

            let neigh = [
                IVec2::new(c.x + 1, c.y),
                IVec2::new(c.x - 1, c.y),
                IVec2::new(c.x, c.y + 1),
                IVec2::new(c.x, c.y - 1),
            ];
            for n_coord in neigh {
                if let Some(n_chunk) = chunk_map.chunks.get(&n_coord) {
                    for sub in 0..SEC_COUNT {
                        let key = (n_coord, sub);
                        if pending_mesh.0.contains_key(&key) { continue; }

                        let y0 = sub * SEC_H;
                        let y1 = (y0 + SEC_H).min(CY);
                        let borders = snapshot_borders(&chunk_map, n_coord, y0, y1);

                        if can_spawn_mesh(&pending_mesh) {
                            let pool = AsyncComputeTaskPool::get();
                            let reg_copy = reg_lite.clone();
                            let chunk_copy = n_chunk.clone();
                            let t = pool.spawn(async move {
                                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                                (key, builds)
                            });
                            pending_mesh.0.insert(key, t);
                        } else {
                            enqueue_mesh(&mut backlog, &pending_mesh, key);
                        }
                    }
                }
            }

            finished.push(*coord);
        }
    }

    for c in finished { pending_gen.0.remove(&c); }
}

fn collect_meshed_subchunks(
    mut commands: Commands,
    mut pending_mesh: ResMut<PendingMesh>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    reg: Res<BlockRegistry>,
    mut chunk_map: ResMut<ChunkMap>,
    q_mesh: Query<&Mesh3d>,
) {
    let mut done_keys = Vec::new();

    for (key, task) in pending_mesh.0.iter_mut() {
        if let Some(((coord, sub), builds)) = future::block_on(future::poll_once(task)) {
            let old_keys: Vec<_> = mesh_index.map
                .keys()
                .cloned()
                .filter(|(c, s, _)| c == &coord && *s as usize == sub)
                .collect();

            despawn_mesh_set(old_keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

            let s = 1.0;
            let origin = Vec3::new(
                (coord.x * CX as i32) as f32 * s,
                (Y_MIN as f32) * s,
                (coord.y * CZ as i32) as f32 * s,
            );

            for (bid, mb) in builds {
                if mb.pos.is_empty() { continue; }
                let mesh = mb.into_mesh();
                let ent = commands.spawn((
                    Mesh3d(meshes.add(mesh)),
                    MeshMaterial3d(reg.material(bid)),
                    Transform::from_translation(origin),
                    SubchunkMesh { coord, sub: sub as u8, block: bid },
                    Name::new(format!("chunk({},{}) sub{} block{}", coord.x, coord.y, sub, bid)),
                )).id();
                mesh_index.map.insert((coord, sub as u8, bid), ent);
            }

            if let Some(chunk) = chunk_map.chunks.get_mut(&coord) {
                chunk.clear_dirty(sub);
            }

            done_keys.push(*key);
        }
    }

    for k in done_keys { pending_mesh.0.remove(&k); }
}

fn schedule_remesh_tasks_from_events(
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    mut backlog: ResMut<MeshBacklog>,
    mut ev_dirty: EventReader<SubchunkDirty>,
) {
    if chunk_map.chunks.is_empty() {
        ev_dirty.clear();
        return;
    }

    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    for e in ev_dirty.read().copied() {
        let coord = e.coord;
        let sub   = e.sub;
        let key   = (coord, sub);

        if pending_mesh.0.contains_key(&key) { continue; }

        let Some(chunk) = chunk_map.chunks.get(&coord) else {
            enqueue_mesh(&mut backlog, &pending_mesh, key);
            continue;
        };

        let y0 = sub * SEC_H;
        let y1 = (y0 + SEC_H).min(CY);
        let borders = snapshot_borders(&chunk_map, coord, y0, y1);

        if can_spawn_mesh(&pending_mesh) {
            let chunk_copy = chunk.clone();
            let reg_copy   = reg_lite.clone();

            let t = pool.spawn(async move {
                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0, Some(borders)).await;
                (key, builds)
            });

            pending_mesh.0.insert(key, t);
        } else {
            enqueue_mesh(&mut backlog, &pending_mesh, key);
        }
    }
}

fn unload_far_chunks(
    mut commands: Commands,
    mut chunk_map: ResMut<ChunkMap>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    q_mesh: Query<&Mesh3d>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    let to_remove: Vec<IVec2> = chunk_map.chunks
        .keys()
        .filter(|coord| {
            (coord.x - center_c.x).abs() > KEEP_RADIUS
                || (coord.y - center_c.y).abs() > KEEP_RADIUS
        })
        .cloned()
        .collect();

    for coord in &to_remove {
        pending_gen.0.remove(coord);
        pending_mesh.0.retain(|(c,_), _| c != coord);

        let keys: Vec<_> = mesh_index.map
            .keys()
            .cloned()
            .filter(|(c, _, _)| c == coord)
            .collect();

        despawn_mesh_set(keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

        chunk_map.chunks.remove(coord);
        backlog.0.retain(|(c, _)| c != coord);
    }
}

#[inline] fn leap(a:f32, b:f32, t:f32) -> f32 { a + (b - a) * t }
#[inline] fn smoothstep(e0:f32, e1:f32, x:f32) -> f32 {
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

#[inline] fn map01(x: f32) -> f32 { x * 0.5 + 0.5 }

pub async fn generate_chunk_async_noise(
    coord: IVec2,
    ids: (BlockId, BlockId, BlockId),
    cfg: WorldGenConfig,
) -> ChunkData {
    let (grass, dirt, stone) = ids;
    let mut c = ChunkData::new();

    let mut height_n = FastNoiseLite::with_seed(cfg.seed);
    height_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    height_n.set_frequency(Option::from(cfg.height_freq));
    height_n.set_fractal_type(Option::from(FractalType::FBm));
    height_n.set_fractal_octaves(Option::from(5));
    height_n.set_fractal_gain(Some(0.5));
    height_n.set_fractal_lacunarity(Some(2.0));

    let mut warp_n = FastNoiseLite::with_seed(cfg.seed ^ 0x5EED_BA5Ei32);
    warp_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    warp_n.set_frequency(Option::from(cfg.warp_freq));

    let mut plains_n = FastNoiseLite::with_seed(cfg.seed ^ 0x0B10Ei32);
    plains_n.set_noise_type(Option::from(NoiseType::OpenSimplex2));
    plains_n.set_frequency(Option::from(cfg.plains_freq));

    let (_cave_n, _cavern_n, _cave_region_n, _entrance_gate_n) = (None::<FastNoiseLite>, None::<FastNoiseLite>, None::<FastNoiseLite>, None::<FastNoiseLite>);

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let wxf = wx as f32; let wzf = wz as f32;

            let mask_raw = map01(plains_n.get_noise_2d(wxf, wzf));
            let plains_factor = smoothstep(
                cfg.plains_threshold - cfg.plains_blend,
                cfg.plains_threshold + cfg.plains_blend,
                mask_raw,
            );

            let amp = leap(cfg.plains_span as f32, cfg.height_span as f32, plains_factor);
            let warp_amp = leap(cfg.warp_amp_plains, cfg.warp_amp, plains_factor);

            // Domain warp
            let dx = warp_n.get_noise_2d(wxf, wzf) * warp_amp;
            let dz = warp_n.get_noise_2d(wxf + 1000.0, wzf - 1000.0) * warp_amp;

            // Basis-Höhe
            let mut h01 = map01(height_n.get_noise_2d(wxf + dx, wzf + dz));

            let flatten = (1.0 - plains_factor) * cfg.plains_flatten;
            if flatten > 0.0 {
                h01 = leap(0.5, h01, 1.0 - flatten);
            }

            let mut h = cfg.base_height + (h01 * amp) as i32;
            h = h.clamp(Y_MIN + 1, Y_MAX - 1);

            for ly in 0..CY {
                let wy = local_y_to_world(ly);

                let id = if wy < h - 2 { stone }
                else if wy < h { dirt }
                else if wy == h { grass }
                else { 0 };

                if id != 0 { c.set(lx, ly, lz, id); }
            }
        }
    }

    c
}

async fn mesh_subchunk_async(
    chunk: &ChunkData,
    reg: &RegLite,
    sub: usize,
    block_size: f32,
    borders: Option<BorderSnapshot>,
) -> Vec<(BlockId, MeshBuild)> {
    let mut by_block: HashMap<BlockId, MeshBuild> = HashMap::new();
    let s = block_size;
    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);

    let (east, west, south, north, snap_y0, _snap_y1) = if let Some(b) = borders {
        debug_assert_eq!(b.y0, y0, "BorderSnapshot.y0 != sub y0");
        debug_assert_eq!(b.y1, y1, "BorderSnapshot.y1 != sub y1");
        (b.east, b.west, b.south, b.north, b.y0, b.y1)
    } else {
        (None, None, None, None, y0, y1)
    };

    let sample = |opt: &Option<Vec<BlockId>>, y: usize, i: usize, stride: usize| -> BlockId {
        match opt {
            Some(v) => {
                let iy = y - snap_y0;
                v[iy * stride + i]
            }
            None => 0,
        }
    };

    let east_at  = |y: usize, z: usize| sample(&east,  y, z, CZ);
    let west_at  = |y: usize, z: usize| sample(&west,  y, z, CZ);
    let south_at = |y: usize, x: usize| sample(&south, y, x, CX);
    let north_at = |y: usize, x: usize| sample(&north, y, x, CX);

    let get = |x:isize,y:isize,z:isize| -> BlockId {
        if x < 0 || y < 0 || z < 0 || x >= CX as isize || y >= CY as isize || z >= CZ as isize { 0 }
        else { chunk.blocks[((y as usize)*CZ + (z as usize))*CX + (x as usize)] }
    };
    let uvq = |u0:f32,v0:f32,u1:f32,v1:f32, flip_v:bool| -> [[f32;2];4] {
        if !flip_v { [[u0,v0],[u1,v0],[u1,v1],[u0,v1]] } else { [[u0,v1],[u1,v1],[u1,v0],[u0,v0]] }
    };

    for y in y0..y1 {
        for z in 0..CZ {
            for x in 0..CX {
                let id = chunk.get(x,y,z);
                if id == 0 { continue; }

                let wx = x as f32 * s; let wy = y as f32 * s; let wz = z as f32 * s;
                let b = by_block.entry(id).or_insert_with(MeshBuild::new);

                // +Y (Top)
                if !(get(x as isize, y as isize+1, z as isize) != 0 && reg.opaque(get(x as isize, y as isize+1, z as isize))) {
                    let u = reg.uv(id, Face::Top);
                    b.quad([[wx,wy+s,wz+s],[wx+s,wy+s,wz+s],[wx+s,wy+s,wz],[wx,wy+s,wz]],[0.0,1.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,false));
                }
                // -Y (Bottom)
                if !(get(x as isize, y as isize-1, z as isize) != 0 && reg.opaque(get(x as isize, y as isize-1, z as isize))) {
                    let u = reg.uv(id, Face::Bottom);
                    b.quad([[wx,wy,wz],[wx+s,wy,wz],[wx+s,wy,wz+s],[wx,wy,wz+s]],[0.0,-1.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,false));
                }
                // +X (East)
                let n_east = if x + 1 < CX { get(x as isize+1, y as isize, z as isize) } else { east_at(y, z) };
                if !(n_east != 0 && reg.opaque(n_east)) {
                    let u = reg.uv(id, Face::East);
                    b.quad([[wx+s,wy,wz+s],[wx+s,wy,wz],[wx+s,wy+s,wz],[wx+s,wy+s,wz+s]],[1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // -X (West)
                let n_west = if x > 0 { get(x as isize-1, y as isize, z as isize) } else { west_at(y, z) };
                if !(n_west != 0 && reg.opaque(n_west)) {
                    let u = reg.uv(id, Face::West);
                    b.quad([[wx,wy,wz],[wx,wy,wz+s],[wx,wy+s,wz+s],[wx,wy+s,wz]],[-1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // +Z (South)
                let n_south = if z + 1 < CZ { get(x as isize, y as isize, z as isize+1) } else { south_at(y, x) };
                if !(n_south != 0 && reg.opaque(n_south)) {
                    let u = reg.uv(id, Face::South);
                    b.quad([[wx,wy,wz+s],[wx+s,wy,wz+s],[wx+s,wy+s,wz+s],[wx,wy+s,wz+s]],[0.0,0.0,1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // -Z (North)
                let n_north = if z > 0 { get(x as isize, y as isize, z as isize-1) } else { north_at(y, x) };
                if !(n_north != 0 && reg.opaque(n_north)) {
                    let u = reg.uv(id, Face::North);
                    b.quad([[wx+s,wy,wz],[wx,wy,wz],[wx,wy+s,wz],[wx+s,wy+s,wz]],[0.0,0.0,-1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
            }
        }
    }

    by_block.into_iter().map(|(k,b)| (k,b)).collect()
}

fn snapshot_borders(chunk_map: &ChunkMap, coord: IVec2, y0: usize, y1: usize) -> BorderSnapshot {
    let mut snap = BorderSnapshot { y0, y1, east: None, west: None, south: None, north: None };

    let take_xz = |c: &ChunkData, x: usize, z: usize, y: usize| -> BlockId { c.get(x,y,z) };

    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x + 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, 0, z, y)); } }
        snap.east = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x - 1, coord.y)) {
        let mut v = Vec::with_capacity((y1 - y0) * CZ);
        for y in y0..y1 { for z in 0..CZ { v.push(take_xz(n, CX-1, z, y)); } }
        snap.west = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y + 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, 0, y)); } }
        snap.south = Some(v);
    }
    if let Some(n) = chunk_map.chunks.get(&IVec2::new(coord.x, coord.y - 1)) {
        let mut v = Vec::with_capacity((y1 - y0) * CX);
        for y in y0..y1 { for x in 0..CX { v.push(take_xz(n, x, CZ-1, y)); } }
        snap.north = Some(v);
    }
    snap
}

fn despawn_mesh_set(
    keys: impl IntoIterator<Item = (IVec2, u8, BlockId)>,
    mesh_index: &mut ChunkMeshIndex,
    commands: &mut Commands,
    q_mesh: &Query<&Mesh3d>,
    meshes: &mut Assets<Mesh>,
) {
    for key in keys {
        if let Some(ent) = mesh_index.map.remove(&key) {
            if let Ok(Mesh3d(handle)) = q_mesh.get(ent) {
                meshes.remove(handle.id());
            }
            commands.entity(ent).despawn();
        }
    }
}

fn can_spawn_mesh(pending_mesh: &PendingMesh) -> bool {
    pending_mesh.0.len() < MAX_INFLIGHT_MESH
}
fn can_spawn_gen(pending_gen: &PendingGen) -> bool {
    pending_gen.0.len() < MAX_INFLIGHT_GEN
}

fn backlog_contains(backlog: &MeshBacklog, key: (IVec2, usize)) -> bool {
    backlog.0.iter().any(|&k| k == key)
}

fn enqueue_mesh(backlog: &mut MeshBacklog, pending: &PendingMesh, key: (IVec2, usize)) {
    if pending.0.contains_key(&key) { return; }
    if backlog_contains(backlog, key) { return; }
    backlog.0.push_back(key);
}