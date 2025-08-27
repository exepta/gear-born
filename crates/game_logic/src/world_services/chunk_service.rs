use std::collections::HashMap;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology, VertexAttributeValues};
use bevy::tasks::{AsyncComputeTaskPool, Task};
use bevy::tasks::futures_lite::future;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{BlockId, BlockRegistry, Face, UvRect};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;

#[derive(Resource, Default)]
pub struct ChunkMeshIndex {
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
        let mut m = Mesh::new(PrimitiveTopology::TriangleList, default());
        m.insert_attribute(Mesh::ATTRIBUTE_POSITION, self.pos);
        m.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   self.nrm);
        m.insert_attribute(Mesh::ATTRIBUTE_UV_0,     self.uv);
        m.insert_indices(Indices::U32(self.idx));
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

pub struct ChunkService;

const LOAD_RADIUS: i32 = 1;
const KEEP_RADIUS: i32 = 2;

impl Plugin for ChunkService {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<ChunkMeshIndex>()
            .init_resource::<PendingGen>()
            .init_resource::<PendingMesh>()
            .add_systems(Update, (
                schedule_chunk_generation,
                collect_generated_chunks,
                collect_meshed_subchunks,
                schedule_remesh_tasks,
                unload_far_chunks,
            ).chain()
                .run_if(in_state(AppState::InGame(InGameStates::Game))));
    }
}

fn schedule_chunk_generation(
    mut pending: ResMut<PendingGen>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    q_cam: Query<&GlobalTransform, With<Camera3d>>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(cam_pos.x.floor() as i32, cam_pos.z.floor() as i32);

    let ids = (
        id_any(&reg, &["grass_block","grass"]),
        id_any(&reg, &["dirt_block","dirt"]),
        id_any(&reg, &["stone_block","stone"]),
    );

    for dz in -LOAD_RADIUS..=LOAD_RADIUS {
        for dx in -LOAD_RADIUS..=LOAD_RADIUS {
            let c = IVec2::new(center_c.x + dx, center_c.y + dz);
            if chunk_map.chunks.contains_key(&c) || pending.0.contains_key(&c) { continue; }

            let pool = AsyncComputeTaskPool::get();
            let ids_copy = ids;
            let task = pool.spawn(async move {
                let data = generate_chunk_async(c, ids_copy).await;
                (c, data)
            });
            pending.0.insert(c, task);
        }
    }
}

fn collect_generated_chunks(
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut chunk_map: ResMut<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    // Snapshot der UVs/Opacity für Meshing-Tasks
    let reg_lite = RegLite::from_reg(&reg);
    let mut finished = Vec::new();

    for (coord, task) in pending_gen.0.iter_mut() {
        if let Some((c, data)) = future::block_on(future::poll_once(task)) {
            chunk_map.chunks.insert(c, data.clone());

            let pool = AsyncComputeTaskPool::get();
            for sub in 0..SEC_COUNT {
                let chunk_copy = data.clone();
                let reg_copy = reg_lite.clone();
                let t = pool.spawn(async move {
                    let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0).await;
                    ((c, sub), builds)
                });
                pending_mesh.0.insert((c, sub), t);
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

            despawn_mesh_set(
                old_keys,
                &mut mesh_index,
                &mut commands,
                &q_mesh,
                &mut meshes,
            );

            let origin = Vec3::new((coord.x * CX as i32) as f32, 0.0, (coord.y * CZ as i32) as f32);

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

fn schedule_remesh_tasks(
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
) {
    if chunk_map.chunks.is_empty() { return; }
    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    for (coord, chunk) in chunk_map.chunks.iter() {
        for sub in 0..SEC_COUNT {
            if !chunk.is_dirty(sub) { continue; }
            if pending_mesh.0.contains_key(&(*coord, sub)) { continue; }

            let chunk_copy = chunk.clone();
            let reg_copy = reg_lite.clone();
            let key = (*coord, sub);
            let t = pool.spawn(async move {
                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, 1.0).await;
                (key, builds)
            });
            pending_mesh.0.insert(key, t);
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

        despawn_mesh_set(
            keys,
            &mut mesh_index,
            &mut commands,
            &q_mesh,
            &mut meshes,
        );

        chunk_map.chunks.remove(coord);
    }
}

async fn generate_chunk_async(coord: IVec2, ids: (BlockId, BlockId, BlockId)) -> ChunkData {
    let (grass, dirt, stone) = ids;
    let mut c = ChunkData::new();

    for lx in 0..CX {
        for lz in 0..CZ {
            let wx = coord.x * CX as i32 + lx as i32;
            let wz = coord.y * CZ as i32 + lz as i32;
            let h = height_at(wx, wz).clamp(Y_MIN + 1, Y_MAX - 1);
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
) -> Vec<(BlockId, MeshBuild)> {
    let mut by_block: HashMap<BlockId, MeshBuild> = HashMap::new();
    let s = block_size;
    let y0 = sub * SEC_H;
    let y1 = (y0 + SEC_H).min(CY);

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
                if !(get(x as isize+1, y as isize, z as isize) != 0 && reg.opaque(get(x as isize+1, y as isize, z as isize))) {
                    let u = reg.uv(id, Face::East);
                    b.quad([[wx+s,wy,wz+s],[wx+s,wy,wz],[wx+s,wy+s,wz],[wx+s,wy+s,wz+s]],[1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // -X (West)
                if !(get(x as isize-1, y as isize, z as isize) != 0 && reg.opaque(get(x as isize-1, y as isize, z as isize))) {
                    let u = reg.uv(id, Face::West);
                    b.quad([[wx,wy,wz],[wx,wy,wz+s],[wx,wy+s,wz+s],[wx,wy+s,wz]],[-1.0,0.0,0.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // +Z (South)
                if !(get(x as isize, y as isize, z as isize+1) != 0 && reg.opaque(get(x as isize, y as isize, z as isize+1))) {
                    let u = reg.uv(id, Face::South);
                    b.quad([[wx,wy,wz+s],[wx+s,wy,wz+s],[wx+s,wy+s,wz+s],[wx,wy+s,wz+s]],[0.0,0.0,1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
                // -Z (North)
                if !(get(x as isize, y as isize, z as isize-1) != 0 && reg.opaque(get(x as isize, y as isize, z as isize-1))) {
                    let u = reg.uv(id, Face::North);
                    b.quad([[wx+s,wy,wz],[wx,wy,wz],[wx,wy+s,wz],[wx+s,wy+s,wz]],[0.0,0.0,-1.0], uvq(u.u0,u.v0,u.u1,u.v1,true));
                }
            }
        }
    }

    by_block.into_iter().map(|(k,b)| (k,b)).collect()
}

#[inline]
fn height_at(x: i32, z: i32) -> i32 {
    let nx = x as f32 * 0.055;
    let nz = z as f32 * 0.045;
    let base = 10.0
        + (nx.sin() * 0.6 + (nx * 0.5 + nz * 0.7).cos() * 0.4) * 6.0
        + (hash2(x, z) * 2.0 - 1.0) * 1.5;
    base.floor() as i32
}

#[inline]
fn hash2(x: i32, z: i32) -> f32 {
    let mut h = x.wrapping_mul(374761393).rotate_left(13) ^ z.wrapping_mul(668265263);
    h = (h ^ (h >> 17)).wrapping_mul(2246822519u32 as i32);
    let v = (h ^ (h >> 15)) as u32;
    (v as f32) / (u32::MAX as f32)
}

#[inline]
fn id_any(reg: &BlockRegistry, names: &[&str]) -> BlockId {
    for n in names {
        if let Some(&id) = reg.name_to_id.get(*n) { return id; }
    }
    0
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