use crate::chunk::chunk_struct::*;
use crate::chunk::chunk_utils::*;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::*;
use bevy::tasks::futures_lite::future;
use bevy::tasks::AsyncComputeTaskPool;
use bevy_rapier3d::prelude::*;
use game_core::configuration::{GameConfig, WorldGenConfig};
use game_core::events::chunk_events::{ChunkUnloadEvent, SubChunkNeedRemeshEvent};
use game_core::states::{AppState, InGameStates, LoadingStates};
use game_core::world::biome::{BiomeEdgeBlend, BiomeRegistry, EdgeMat};
use game_core::world::block::{BlockId, BlockRegistry, VOXEL_SIZE};
use game_core::world::chunk::*;
use game_core::world::chunk_dim::*;
use game_core::world::region::BiomeRegionAllocator;
use game_core::world::save::{RegionCache, WorldSave};
use game_core::BlockCatalogPreviewCam;
use std::collections::{HashMap, HashSet};

const MAX_COLLIDERS_PER_FRAME: usize = 12;

#[derive(Default, Resource)]
struct ColliderBacklog(Vec<ColliderTodo>);

struct ColliderTodo {
    coord: IVec2,
    sub: u8,
    origin: Vec3,
    positions: Vec<[f32;3]>,
    indices: Vec<u32>,
}

#[derive(Resource, Default)]
struct ChunkColliderIndex(pub HashMap<(IVec2, u8), Entity>);

#[derive(Resource, Default)]
struct KickQueue(Vec<KickItem>);

#[derive(Clone, Copy, Debug)]
struct KickItem {
    coord: IVec2,
    sub:   u8,
    frames_left: u8,
    tries_left:  u8,
}

#[derive(Resource, Default)]
struct KickedOnce(HashSet<(IVec2, u8)>);

#[derive(Resource, Default)]
struct QueuedOnce(HashSet<(IVec2, u8)>);

pub struct ChunkBuilder;

impl Plugin for ChunkBuilder {
    fn build(&self, app: &mut App) {
        app
            .init_resource::<ChunkMeshIndex>()
            .init_resource::<MeshBacklog>()
            .init_resource::<PendingGen>()
            .init_resource::<PendingMesh>()
            .init_resource::<ChunkColliderIndex>()
            .init_resource::<ColliderBacklog>()
            .init_resource::<KickQueue>()
            .init_resource::<KickedOnce>()
            .init_resource::<QueuedOnce>()
            .add_systems(Update, (
                schedule_chunk_generation,
                collect_generated_chunks,
                collect_meshed_subchunks,

                enqueue_kick_for_new_subchunks,
                process_kick_queue,

                schedule_remesh_tasks_from_events.in_set(VoxelStage::Meshing),
                drain_mesh_backlog,
                unload_far_chunks,
                cleanup_kick_flags_on_unload.after(unload_far_chunks)
            )
                .chain()
                .run_if(in_state(AppState::Loading(LoadingStates::BaseGen))
                    .or(in_state(AppState::InGame(InGameStates::Game)))));

        app.add_systems(
            Update,
            check_base_gen_world_ready
                .run_if(in_state(AppState::Loading(LoadingStates::BaseGen)))
        );

        app.add_systems(
            Update,
            drain_collider_backlog
                .after(collect_meshed_subchunks)
                .run_if(
                    in_state(AppState::Loading(LoadingStates::BaseGen))
                        .or(in_state(AppState::InGame(InGameStates::Game)))
                )
        );
    }
}

// ================================================
//                    Sub Update
// ================================================

fn enqueue_kick_for_new_subchunks(
    q_new_meshes: Query<&SubchunkMesh, Added<SubchunkMesh>>,
    mut queue: ResMut<KickQueue>,
    kicked: Res<KickedOnce>,
    mut queued: ResMut<QueuedOnce>,
) {
    let mut seen: HashSet<(IVec2, u8)> = HashSet::new();

    for m in q_new_meshes.iter() {
        let key = (m.coord, m.sub);

        if kicked.0.contains(&key) { continue; }

        if !seen.insert(key) { continue; }

        if queued.0.contains(&key) { continue; }

        queue.0.push(KickItem { coord: m.coord, sub: m.sub, frames_left: 3, tries_left: 8 });
        queued.0.insert(key);
    }
}

fn process_kick_queue(
    mut queue: ResMut<KickQueue>,
    mut kicked: ResMut<KickedOnce>,
    mut queued: ResMut<QueuedOnce>,
    chunk_map: Res<ChunkMap>,
    mut ev_dirty: EventWriter<SubChunkNeedRemeshEvent>,
) {
    let mut i = 0;
    while i < queue.0.len() {
        let item = &mut queue.0[i];

        if item.frames_left > 0 {
            item.frames_left -= 1;
            i += 1;
            continue;
        }

        if !chunk_map.chunks.contains_key(&item.coord) {
            queued.0.remove(&(item.coord, item.sub));
            queue.0.swap_remove(i);
            continue;
        }

        if neighbors_ready(&chunk_map, item.coord) {
            ev_dirty.write(SubChunkNeedRemeshEvent { coord: item.coord, sub: item.sub as usize });
            kicked.0.insert((item.coord, item.sub));
            queued.0.remove(&(item.coord, item.sub));
            queue.0.swap_remove(i);
            continue;
        }

        if item.tries_left > 0 {
            item.frames_left = 3;
            item.tries_left -= 1;
            i += 1;
        } else {
            queued.0.remove(&(item.coord, item.sub));
            queue.0.swap_remove(i);
        }
    }
}
// ================================================
//                    Main
// ================================================

fn check_base_gen_world_ready(
    game_config: Res<GameConfig>,
    load_center: Res<LoadCenter>,
    chunk_map: Res<ChunkMap>,
    pending_gen: Res<PendingGen>,
    pending_mesh: Res<PendingMesh>,
    backlog: Res<MeshBacklog>,
    mut next: ResMut<NextState<AppState>>,
    mut commands: Commands,
) {
    let initial_radius = game_config.graphics.chunk_range.min(3);

    if area_ready(load_center.world_xz, initial_radius, &chunk_map, &pending_gen, &pending_mesh, &backlog) {
        commands.remove_resource::<LoadCenter>();
        next.set(AppState::Loading(LoadingStates::WaterGen));
    }
}

//System
fn schedule_chunk_generation(
    mut pending: ResMut<PendingGen>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    biome_reg: Res<BiomeRegistry>,
    mut region_alloc: ResMut<BiomeRegionAllocator>,
    gen_cfg: Res<WorldGenConfig>,
    game_config: Res<GameConfig>,
    ws: Res<WorldSave>,
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    load_center: Option<Res<LoadCenter>>,
    app_state: Res<State<AppState>>,
) {
    // --- helpers (hash, rng, weighted pick) ---
    #[inline]
    fn hash64(x: i32, z: i32, seed: i32) -> u64 {
        let xu = x as u32 as u64;
        let zu = z as u32 as u64;
        let su = seed as u32 as u64;
        let mut v = xu.wrapping_mul(0x9E37_79B9_7F4A_7C15)
            ^ zu.wrapping_mul(0xC2B2_AE3D_27D4_EB4F)
            ^ su.rotate_left(13);
        v ^= v >> 30;
        v = v.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        v ^= v >> 27;
        v = v.wrapping_mul(0x94D0_49BB_1331_11EB);
        v ^ (v >> 31)
    }

    #[inline]
    fn f_rand01(x: i32, z: i32, seed: i32, salt: u64) -> f32 {
        let h = hash64(x, z, seed) ^ salt;
        let m = ((h >> 40) & 0xFF_FFFF) as u32;
        (m as f32) / 16_777_216.0
    }

    fn pick_weighted(items: &[(String, f32)], r01: f32) -> Option<&(String, f32)> {
        if items.is_empty() { return None; }
        let sum: f32 = items.iter().map(|(_, w)| *w).sum();
        let mut t = r01 * sum;
        for it in items {
            if t <= it.1 { return Some(it); }
            t -= it.1;
        }
        items.last()
    }

    // Resolve (top,bottom, stone,seafloor, beach) by biome name
    fn resolve_surface_ids_for_biome(
        blocks: &BlockRegistry,
        biome_reg: &BiomeRegistry,
        biome_name: &str,
        world_seed: i32,
        c: IVec2,
    ) -> (BlockId, BlockId, BlockId, BlockId, BlockId) {
        let grass_fb = blocks.id_or_air("grass_block");
        let dirt_fb  = blocks.id_or_air("dirt_block");
        let stone_fb = blocks.id_or_air("stone_block");
        let sand_fb  = blocks.id_or_air("sand_block");

        let b = if let Some(b) = biome_reg.get(biome_name) { b } else {
            return (grass_fb, dirt_fb, stone_fb, sand_fb, sand_fb);
        };

        let top_id = b.surface.top.get(0)
            .map(|bc| blocks.id_or_air(&bc.id))
            .unwrap_or(grass_fb);

        let bottom_id = b.surface.bottom.get(0)
            .map(|bc| blocks.id_or_air(&bc.id))
            .unwrap_or(dirt_fb);

        let stone_id = b.surface.under_zero.get(0)
            .or_else(|| b.surface.upper_zero.get(0))
            .map(|bc| blocks.id_or_air(&bc.id))
            .unwrap_or(stone_fb);

        let mut weighted: Vec<(String, f32)> = Vec::new();
        for bc in &b.surface.sea_floor {
            let w = if bc.weight > 0.0 { bc.weight } else { 1.0 };
            weighted.push((bc.id.clone(), w));
        }
        let seafloor_id = if let Some((name, _w)) = pick_weighted(&weighted, f_rand01(c.x, c.y, world_seed, 0x5EA5_EAF1)) {
            blocks.id_or_air(name)
        } else {
            b.surface.sea_floor.get(0)
                .map(|bc| blocks.id_or_air(&bc.id))
                .unwrap_or(sand_fb)
        };

        let beach_id = sand_fb;
        (top_id, bottom_id, stone_id, seafloor_id, beach_id)
    }

    // Stable salt for a (biomeA, biomeB) border (order-independent).
    fn salt_for_pair(a: &str, b: &str) -> u32 {
        let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
        let mut h: u32 = 0x811C9DC5;
        for &ch in lo.as_bytes() { h ^= ch as u32; h = h.wrapping_mul(0x0100_0193); }
        h ^= 0x9E37_79B9;
        for &ch in hi.as_bytes() { h ^= ch as u32; h = h.wrapping_mul(0x0100_0193); }
        h
    }

    // Decide which biome "wins" the border. Special rule: Plains always lose to non-Plains.
    fn border_winner<'a>(a: &'a str, b: &'a str) -> &'a str {
        if a == "Plains" && b != "Plains" { return b; }
        if b == "Plains" && a != "Plains" { return a; }
        // deterministic fallback by pair-hash parity (order-independent)
        let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
        let mut h: u32 = 0x811C9DC5;
        for &ch in lo.as_bytes() { h ^= ch as u32; h = h.wrapping_mul(0x0100_0193); }
        h ^= 0x9E37_79B9;
        for &ch in hi.as_bytes() { h ^= ch as u32; h = h.wrapping_mul(0x0100_0193); }
        if (h & 1) == 0 { lo } else { hi }
    }

    // --- Original flow ---
    let center_c = if let Ok(t) = q_cam.single() {
        let (c, _) = world_to_chunk_xz(
            (t.translation().x / VOXEL_SIZE).floor() as i32,
            (t.translation().z / VOXEL_SIZE).floor() as i32
        );
        c
    } else if let Some(lc) = load_center {
        lc.world_xz
    } else {
        IVec2::ZERO
    };

    let waiting = is_waiting(&app_state);
    let max_inflight = if waiting { BIG } else { MAX_INFLIGHT_GEN };
    let per_frame_submit = if waiting { BIG } else { 8 };

    let load_radius = game_config.graphics.chunk_range;
    let mut budget = max_inflight.saturating_sub(pending.0.len()).min(per_frame_submit);

    let cfg_clone = gen_cfg.clone();
    let ws_root = ws.root.clone();
    let pool = AsyncComputeTaskPool::get();

    for dz in -load_radius..=load_radius {
        for dx in -load_radius..=load_radius {
            if budget == 0 { return; }
            let c = IVec2::new(center_c.x + dx, center_c.y + dz);
            if chunk_map.chunks.contains_key(&c) || pending.0.contains_key(&c) { continue; }

            // Primary biome for this chunk (chunk → biome).
            let biome_name = if let Some(name) = region_alloc.biome_for_chunk(c, &biome_reg) {
                name
            } else {
                biome_reg.iter().next().map(|(_, b)| b.name.clone()).unwrap_or_else(|| "Plains".to_string())
            };

            // Resolve IDs for primary biome
            let ids = resolve_surface_ids_for_biome(&reg, &biome_reg, &biome_name, gen_cfg.seed, c);

            // Build blend info: only set for edges where the **neighbor biome wins**.
            let mut blend = BiomeEdgeBlend { radius: 18, ..Default::default() };
            let rid_here = region_alloc.region_id_of(c);

            // west neighbor
            {
                let cw = IVec2::new(c.x - 1, c.y);
                if region_alloc.region_id_of(cw) != rid_here {
                    if let Some(nei_name) = region_alloc.biome_for_chunk(cw, &biome_reg) {
                        if border_winner(&biome_name, &nei_name) == nei_name {
                            let (top, bottom, _, _, _) =
                                resolve_surface_ids_for_biome(&reg, &biome_reg, &nei_name, gen_cfg.seed, cw);
                            let salt = salt_for_pair(&biome_name, &nei_name);
                            blend.west = Some(EdgeMat { top, bottom, salt });
                        }
                    }
                }
            }
            // east neighbor
            {
                let ce = IVec2::new(c.x + 1, c.y);
                if region_alloc.region_id_of(ce) != rid_here {
                    if let Some(nei_name) = region_alloc.biome_for_chunk(ce, &biome_reg) {
                        if border_winner(&biome_name, &nei_name) == nei_name {
                            let (top, bottom, _, _, _) =
                                resolve_surface_ids_for_biome(&reg, &biome_reg, &nei_name, gen_cfg.seed, ce);
                            let salt = salt_for_pair(&biome_name, &nei_name);
                            blend.east = Some(EdgeMat { top, bottom, salt });
                        }
                    }
                }
            }
            // north neighbor
            {
                let cn = IVec2::new(c.x, c.y - 1);
                if region_alloc.region_id_of(cn) != rid_here {
                    if let Some(nei_name) = region_alloc.biome_for_chunk(cn, &biome_reg) {
                        if border_winner(&biome_name, &nei_name) == nei_name {
                            let (top, bottom, _, _, _) =
                                resolve_surface_ids_for_biome(&reg, &biome_reg, &nei_name, gen_cfg.seed, cn);
                            let salt = salt_for_pair(&biome_name, &nei_name);
                            blend.north = Some(EdgeMat { top, bottom, salt });
                        }
                    }
                }
            }
            // south neighbor
            {
                let cs = IVec2::new(c.x, c.y + 1);
                if region_alloc.region_id_of(cs) != rid_here {
                    if let Some(nei_name) = region_alloc.biome_for_chunk(cs, &biome_reg) {
                        if border_winner(&biome_name, &nei_name) == nei_name {
                            let (top, bottom, _, _, _) =
                                resolve_surface_ids_for_biome(&reg, &biome_reg, &nei_name, gen_cfg.seed, cs);
                            let salt = salt_for_pair(&biome_name, &nei_name);
                            blend.south = Some(EdgeMat { top, bottom, salt });
                        }
                    }
                }
            }

            // Spawn async gen; clone registry so the future is 'static
            let cfg = cfg_clone.clone();
            let root = ws_root.clone();
            let ids_copy = ids;
            let blend_copy = blend;
            let reg_owned = (*reg).clone();

            let task = pool.spawn(async move {
                let data = load_or_gen_chunk_async(root, c, ids_copy, blend_copy, &reg_owned, cfg).await;
                (c, data)
            });
            pending.0.insert(c, task);
            budget -= 1;
        }
    }
}

//System
fn drain_mesh_backlog(
    mut backlog: ResMut<MeshBacklog>,
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    app_state: Res<State<AppState>>,
) {
    if chunk_map.chunks.is_empty() { backlog.0.clear(); return; }

    let waiting = is_waiting(&app_state);
    let max_inflight_mesh = if waiting { BIG } else { MAX_INFLIGHT_MESH };

    let reg_lite = RegLite::from_reg(&reg);
    let pool = AsyncComputeTaskPool::get();

    while pending_mesh.0.len() < max_inflight_mesh {
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
            let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, VOXEL_SIZE, Some(borders)).await;
            (key, builds)
        });
        pending_mesh.0.insert(key, t);
    }
}

//System
fn collect_generated_chunks(
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    mut chunk_map: ResMut<ChunkMap>,
    reg: Res<BlockRegistry>,
    app_state: Res<State<AppState>>,
) {
    let waiting = is_waiting(&app_state);
    let max_inflight_mesh = if waiting { BIG } else { MAX_INFLIGHT_MESH };

    let reg_lite = RegLite::from_reg(&reg);
    let mut finished = Vec::new();

    for (coord, task) in pending_gen.0.iter_mut() {
        if let Some((c, data)) = future::block_on(future::poll_once(task)) {
            chunk_map.chunks.insert(c, data.clone());

            let pool = AsyncComputeTaskPool::get();
            let order = sub_priority_order(&data);
            for sub in order {
                let key = (c, sub);
                let y0 = sub * SEC_H;
                let y1 = (y0 + SEC_H).min(CY);
                let borders = snapshot_borders(&chunk_map, c, y0, y1);

                if pending_mesh.0.len() < max_inflight_mesh {
                    let chunk_copy = data.clone();
                    let reg_copy   = reg_lite.clone();
                    let t = pool.spawn(async move {
                        let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, VOXEL_SIZE, Some(borders)).await;
                        ((c, sub), builds)
                    });
                    pending_mesh.0.insert(key, t);
                } else {
                    enqueue_mesh(&mut backlog, &pending_mesh, key);
                }
            }

            for n_coord in neighbors4_iter(c) {
                if let Some(n_chunk) = chunk_map.chunks.get(&n_coord) {
                    let order_n = sub_priority_order(n_chunk);
                    for sub in order_n {
                        let key = (n_coord, sub);
                        if pending_mesh.0.contains_key(&key) { continue; }

                        let y0 = sub * SEC_H;
                        let y1 = (y0 + SEC_H).min(CY);
                        let borders = snapshot_borders(&chunk_map, n_coord, y0, y1);

                        if pending_mesh.0.len() < max_inflight_mesh {
                            let pool = AsyncComputeTaskPool::get();
                            let reg_copy  = reg_lite.clone();
                            let chunk_copy = n_chunk.clone();
                            let t = pool.spawn(async move {
                                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, VOXEL_SIZE, Some(borders)).await;
                                ((n_coord, sub), builds)
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

    for c in finished {
        pending_gen.0.remove(&c);
    }
}

//System
fn collect_meshed_subchunks(
    mut commands: Commands,
    mut pending_mesh: ResMut<PendingMesh>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut collider_index: ResMut<ChunkColliderIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    reg: Res<BlockRegistry>,
    mut chunk_map: ResMut<ChunkMap>,
    q_mesh: Query<&Mesh3d>,
    app_state: Res<State<AppState>>,
) {
    let waiting = is_waiting(&app_state);
    let apply_cap = if waiting { BIG } else { MAX_UPDATE_FRAMES };
    let mut done_keys = Vec::new();
    let mut applied = 0usize;

    let mut collider_budget = if waiting { BIG } else { MAX_COLLIDERS_PER_FRAME };

    for (key, task) in pending_mesh.0.iter_mut() {
        if applied >= apply_cap { break; }

        if let Some(((coord, sub), builds)) = future::block_on(future::poll_once(task)) {
            let old_keys: Vec<_> = mesh_index
                .map
                .keys()
                .cloned()
                .filter(|(c, s, _)| c == &coord && *s as usize == sub)
                .collect();
            despawn_mesh_set(old_keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

            if let Some(ent) = collider_index.0.remove(&(coord, sub as u8)) {
                commands.entity(ent).despawn();
            }

            let s = VOXEL_SIZE;
            let origin = Vec3::new(
                (coord.x * CX as i32) as f32 * s,
                (Y_MIN as f32) * s,
                (coord.y * CZ as i32) as f32 * s,
            );

            let mut phys_positions: Vec<[f32; 3]> = Vec::new();
            let mut phys_indices:   Vec<u32>       = Vec::new();

            for (bid, mb) in builds {
                if mb.pos.is_empty() { continue; }

                let base = phys_positions.len() as u32;
                phys_positions.extend_from_slice(&mb.pos);
                phys_indices.extend(mb.idx.iter().map(|i| base + *i));

                let mesh = mb.into_mesh();
                let ent = commands
                    .spawn((
                        Mesh3d(meshes.add(mesh)),
                        MeshMaterial3d(reg.material(bid)),
                        Transform::from_translation(origin),
                        SubchunkMesh { coord, sub: sub as u8, block: bid },
                        Name::new(format!("chunk({},{}) sub{} block{}", coord.x, coord.y, sub, bid)),
                    ))
                    .id();
                mesh_index.map.insert((coord, sub as u8, bid), ent);
            }

            if !phys_positions.is_empty() && collider_budget > 0 {
                let mut pm = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
                pm.insert_attribute(Mesh::ATTRIBUTE_POSITION, phys_positions);
                pm.insert_indices(Indices::U32(phys_indices));

                let flags = TriMeshFlags::FIX_INTERNAL_EDGES
                    | TriMeshFlags::MERGE_DUPLICATE_VERTICES
                    | TriMeshFlags::DELETE_DEGENERATE_TRIANGLES
                    | TriMeshFlags::ORIENTED;

                if let Some(collider) = Collider::from_bevy_mesh(&pm, &ComputedColliderShape::TriMesh(flags)) {
                    let cent = commands
                        .spawn((
                            RigidBody::Fixed,
                            collider,
                            Transform::from_translation(origin),
                            Name::new(format!("collider chunk({},{}) sub{}", coord.x, coord.y, sub)),
                        ))
                        .id();
                    collider_index.0.insert((coord, sub as u8), cent);
                    collider_budget = collider_budget.saturating_sub(1);
                }
            }

            if let Some(chunk) = chunk_map.chunks.get_mut(&coord) {
                chunk.clear_dirty(sub);
            }

            applied += 1;
            done_keys.push(*key);
        }
    }

    for k in done_keys {
        pending_mesh.0.remove(&k);
    }
}

//System
fn schedule_remesh_tasks_from_events(
    mut pending_mesh: ResMut<PendingMesh>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    mut backlog: ResMut<MeshBacklog>,
    mut ev_dirty: EventReader<SubChunkNeedRemeshEvent>,
    app_state: Res<State<AppState>>,
) {
    if chunk_map.chunks.is_empty() {
        ev_dirty.clear();
        return;
    }

    let waiting = is_waiting(&app_state);
    let max_inflight_mesh = if waiting { BIG } else { MAX_INFLIGHT_MESH };

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

        if pending_mesh.0.len() < max_inflight_mesh {
            let chunk_copy = chunk.clone();
            let reg_copy   = reg_lite.clone();

            let t = pool.spawn(async move {
                let builds = mesh_subchunk_async(&chunk_copy, &reg_copy, sub, VOXEL_SIZE, Some(borders)).await;
                (key, builds)
            });

            pending_mesh.0.insert(key, t);
        } else {
            enqueue_mesh(&mut backlog, &pending_mesh, key);
        }
    }
}

//System
fn unload_far_chunks(
    mut commands: Commands,
    mut chunk_map: ResMut<ChunkMap>,
    mut mesh_index: ResMut<ChunkMeshIndex>,
    mut collider_index: ResMut<ChunkColliderIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut pending_gen: ResMut<PendingGen>,
    mut pending_mesh: ResMut<PendingMesh>,
    mut backlog: ResMut<MeshBacklog>,
    game_config: Res<GameConfig>,
    ws: Res<WorldSave>,
    mut cache: ResMut<RegionCache>,
    q_mesh: Query<&Mesh3d>,
    q_cam: Query<&GlobalTransform, (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    mut ev_water_unload: EventWriter<ChunkUnloadEvent>,
    mut coll_backlog: ResMut<ColliderBacklog>,
) {
    let cam = if let Ok(t) = q_cam.single() { t } else { return; };
    let cam_pos = cam.translation();
    let (center_c, _) = world_to_chunk_xz(
        (cam_pos.x / VOXEL_SIZE).floor() as i32,
        (cam_pos.z / VOXEL_SIZE).floor() as i32,
    );

    let keep_radius = game_config.graphics.chunk_range + 1;

    let to_remove: Vec<IVec2> = chunk_map
        .chunks
        .keys()
        .filter(|coord| {
            (coord.x - center_c.x).abs() > keep_radius
                || (coord.y - center_c.y).abs() > keep_radius
        })
        .cloned()
        .collect();

    for coord in &to_remove {
        if let Some(chunk) = chunk_map.chunks.get(coord) {
            let _ = save_chunk_sync(&ws, &mut cache, *coord, chunk);
        }

        pending_gen.0.remove(coord);
        pending_mesh.0.retain(|(c, _), _| c != coord);

        let old_keys: Vec<_> = mesh_index
            .map
            .keys()
            .cloned()
            .filter(|(c, _, _)| c == coord)
            .collect();
        despawn_mesh_set(old_keys, &mut mesh_index, &mut commands, &q_mesh, &mut meshes);

        let col_keys: Vec<_> = collider_index
            .0
            .keys()
            .cloned()
            .filter(|(c, _)| c == coord)
            .collect();
        for k in col_keys {
            if let Some(ent) = collider_index.0.remove(&k) {
                commands.entity(ent).despawn();
            }
        }

        ev_water_unload.write(ChunkUnloadEvent { coord: *coord });

        chunk_map.chunks.remove(coord);
        backlog.0.retain(|(c, _)| c != coord);
        coll_backlog.0.retain(|t| t.coord != *coord);
    }
}

fn cleanup_kick_flags_on_unload(
    mut ev_unload: EventReader<ChunkUnloadEvent>,
    mut kicked: ResMut<KickedOnce>,
    mut queued: ResMut<QueuedOnce>,
    mut queue: ResMut<KickQueue>,
) {
    for e in ev_unload.read() {
        let coord = e.coord;
        kicked.0.retain(|(c, _)| *c != coord);
        queued.0.retain(|(c, _)| *c != coord);
        queue.0.retain(|it| it.coord != coord);
    }
}

fn drain_collider_backlog(
    mut commands: Commands,
    mut backlog: ResMut<ColliderBacklog>,
    mut collider_index: ResMut<ChunkColliderIndex>,
    mut collider_budget: Local<usize>,
) {
    if *collider_budget == 0 {
        *collider_budget = MAX_COLLIDERS_PER_FRAME;
    }

    let flags = TriMeshFlags::FIX_INTERNAL_EDGES
        | TriMeshFlags::MERGE_DUPLICATE_VERTICES
        | TriMeshFlags::DELETE_DEGENERATE_TRIANGLES
        | TriMeshFlags::ORIENTED;

    let i = 0;
    while i < backlog.0.len() && *collider_budget > 0 {
        let todo = backlog.0.remove(i);

        if collider_index.0.contains_key(&(todo.coord, todo.sub)) {
            continue;
        }

        let mut pm = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
        pm.insert_attribute(Mesh::ATTRIBUTE_POSITION, todo.positions);
        pm.insert_indices(Indices::U32(todo.indices));

        if let Some(coll) = Collider::from_bevy_mesh(&pm, &ComputedColliderShape::TriMesh(flags)) {
            let cent = commands
                .spawn((
                    RigidBody::Fixed,
                    coll,
                    Transform::from_translation(todo.origin),
                    Name::new(format!("collider chunk({},{}) sub{}", todo.coord.x, todo.coord.y, todo.sub)),
                ))
                .id();
            collider_index.0.insert((todo.coord, todo.sub), cent);
            *collider_budget -= 1;
        }
    }
}

#[inline]
fn estimate_surface_sub_fast(chunk: &ChunkData) -> usize {
    let mut max_wy = Y_MIN - 1;
    for z in (0..CZ).step_by(4) {
        for x in (0..CX).step_by(4) {
            for ly in (0..CY).rev() {
                if chunk.get(x, ly, z) != 0 {
                    let wy = Y_MIN + ly as i32;
                    if wy > max_wy { max_wy = wy; }
                    break;
                }
            }
        }
    }
    let ly = (max_wy - Y_MIN).max(0) as usize;
    (ly / SEC_H).clamp(0, SEC_COUNT.saturating_sub(1))
}

fn sub_priority_order(chunk: &ChunkData) -> Vec<usize> {
    let mut out = Vec::with_capacity(SEC_COUNT);
    let mut used = vec![false; SEC_COUNT];
    let mid = estimate_surface_sub_fast(chunk);

    out.push(mid); used[mid] = true;

    let mut off = 1isize;
    while out.len() < SEC_COUNT {
        let below = mid as isize - off;
        if below >= 0 && !used[below as usize] {
            out.push(below as usize);
            used[below as usize] = true;
        }
        let above = mid as isize + off;
        if above < SEC_COUNT as isize && !used[above as usize] {
            out.push(above as usize);
            used[above as usize] = true;
        }
        off += 1;
    }
    out
}

