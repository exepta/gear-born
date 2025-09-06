use crate::world::chunk::ChunkMap;
use crate::world::chunk_dim::{world_to_chunk_xz, Y_MAX, Y_MIN};
use crate::world::fluid::FluidMap;
use crate::world::world_access_mut;

use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/* ---------------- constants ---------------- */

pub const VOXEL_SIZE: f32 = 1.0;
const ATLAS_PAD_PX: f32 = 0.5;

pub const BASE_BREAK_TIME: f32 = 0.55;
pub const PER_HARDNESS: f32 = 0.45;

pub const MIN_BREAK_TIME: f32 = 0.2;
pub const MAX_BREAK_TIME: f32 = 60.0;

/* ---------------- types ---------------- */

pub type BlockId = u16;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Face { Top, Bottom, North, East, South, West }

#[derive(Clone, Copy)]
pub struct UvRect { pub u0:f32, pub v0:f32, pub u1:f32, pub v1:f32 }

#[derive(Clone)]
pub struct BlockDef {
    pub name: String,
    pub stats: BlockStats,
    pub uv_top: UvRect,
    pub uv_bottom: UvRect,
    pub uv_north: UvRect,
    pub uv_east: UvRect,
    pub uv_south: UvRect,
    pub uv_west: UvRect,
    pub image: Handle<Image>,
    pub material: Handle<StandardMaterial>,
}

#[derive(Deserialize, Clone, Default)]
#[allow(dead_code)]
pub struct BlockStats {
    #[serde(default)]
    pub hardness: f32,
    #[serde(default)]
    pub blast_resistance: f32,
    #[serde(default = "d_true")]
    pub opaque: bool,
    #[serde(default)]
    pub fluid: bool,
    #[serde(default)]
    pub emissive: f32,
}

#[derive(Resource, Clone, Debug)]
pub struct SelectedBlock {
    pub id:   u16,
    pub name: String,
}

impl Default for SelectedBlock {
    fn default() -> Self { Self { id: 0, name: "air".to_string() } }
}

/* ---------------- registry ---------------- */

#[derive(Resource)]
pub struct BlockRegistry {
    pub defs: Vec<BlockDef>,
    pub name_to_id: HashMap<String, BlockId>,
}

impl BlockRegistry {
    #[inline] pub fn def(&self, id: BlockId) -> &BlockDef { &self.defs[id as usize] }
    #[inline] pub fn name(&self, id: BlockId) -> &str { self.def(id).name.as_str() }

    #[inline] pub fn name_opt(&self, id: BlockId) -> Option<&str> {
        self.defs.get(id as usize).map(|d| d.name.as_str())
    }

    #[inline] pub fn id_opt(&self, name: &str) -> Option<BlockId> {
        self.name_to_id.get(name).copied()
    }

    #[inline] pub fn id(&self, name:&str) -> BlockId {
        *self.name_to_id.get(name).expect("unknown block name")
    }

    #[inline] pub fn id_or_air(&self, name:&str) -> BlockId {
        self.id_opt(name).unwrap_or(0)
    }

    #[inline] pub fn material(&self, id: BlockId) -> Handle<StandardMaterial> {
        self.def(id).material.clone()
    }

    #[inline] pub fn stats(&self, id: BlockId) -> &BlockStats { &self.def(id).stats }
    #[inline] pub fn is_air(&self, id: BlockId) -> bool { id == 0 }
    #[inline] pub fn is_opaque(&self, id: BlockId) -> bool { self.stats(id).opaque }
    #[inline] pub fn is_fluid(&self, id: BlockId) -> bool { self.stats(id).fluid }
    #[inline] pub fn emissive(&self, id: BlockId) -> f32 { self.stats(id).emissive }
    #[inline] pub fn hardness(&self, id: BlockId) -> f32 { self.stats(id).hardness.max(0.0) }

    #[inline]
    pub fn uv(&self, id: BlockId, face: Face) -> UvRect {
        let d = self.def(id);
        match face {
            Face::Top => d.uv_top,
            Face::Bottom => d.uv_bottom,
            Face::North => d.uv_north,
            Face::East => d.uv_east,
            Face::South => d.uv_south,
            Face::West => d.uv_west,
        }
    }

    #[inline]
    pub fn face_uvs(&self, id: BlockId) -> (UvRect, UvRect, UvRect, UvRect, UvRect, UvRect) {
        let d = self.def(id);
        (d.uv_top, d.uv_bottom, d.uv_north, d.uv_east, d.uv_south, d.uv_west)
    }

    pub fn load_all(
        asset_server: &AssetServer,
        materials: &mut Assets<StandardMaterial>,
        blocks_dir: &str,
    ) -> Self {
        // 0 = air
        let mut defs: Vec<BlockDef> = Vec::new();
        let mut name_to_id = HashMap::new();

        defs.push(BlockDef {
            name: "air".into(),
            stats: BlockStats::default(),
            uv_top: Z, uv_bottom: Z, uv_north: Z, uv_east: Z, uv_south: Z, uv_west: Z,
            image: Handle::default(),
            material: Handle::default(),
        });
        name_to_id.insert("air".into(), 0);

        let dir = Path::new(blocks_dir);
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().and_then(|s| s.to_str()) != Some("json") { continue; }

            let block_json: BlockJson = read_json(path.to_str().unwrap());
            let tex_dir = block_json
                .texture_dir
                .clone()
                .unwrap_or_else(|| guess_tex_dir_from_block_name(&block_json.name));

            // tileset is read from disk (not via asset server)
            let tileset_path = format!("assets/{}/data.json", tex_dir);
            let tileset: BlockTileset = read_json(&tileset_path);

            // atlas image is loaded via asset_server
            let atlas_path = format!("{}/{}", tex_dir, tileset.image);
            let image: Handle<Image> = asset_server.load(atlas_path.as_str());

            // resolve faces (supports: specific keys, 'all', 'vertical', 'horizontal', and 'nord' fallback)
            let faces = block_json.texture.resolve();
            let uv_top    = tile_uv(&tileset, require_face(&faces.top,    "top",    &block_json.name)).unwrap();
            let uv_bottom = tile_uv(&tileset, require_face(&faces.bottom, "bottom", &block_json.name)).unwrap();
            let uv_north  = tile_uv(&tileset, require_face(&faces.north,  "north",  &block_json.name)).unwrap();
            let uv_east   = tile_uv(&tileset, require_face(&faces.east,   "east",   &block_json.name)).unwrap();
            let uv_south  = tile_uv(&tileset, require_face(&faces.south,  "south",  &block_json.name)).unwrap();
            let uv_west   = tile_uv(&tileset, require_face(&faces.west,   "west",   &block_json.name)).unwrap();

            let (alpha_mode, base_color) = material_policy_from_stats(&block_json.stats);

            let material = materials.add(StandardMaterial {
                base_color_texture: Some(image.clone()),
                base_color,
                alpha_mode,
                unlit: false,
                metallic: 0.0,
                perceptual_roughness: 1.0,
                reflectance: 0.0,
                ..Default::default()
            });

            let id = defs.len() as BlockId;
            name_to_id.insert(block_json.name.clone(), id);
            defs.push(BlockDef {
                name: block_json.name,
                stats: block_json.stats,
                uv_top, uv_bottom, uv_north, uv_east, uv_south, uv_west,
                image,
                material,
            });
        }

        Self { defs, name_to_id }
    }
}

/* ---------------- optional enum helpers ---------------- */

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Blocks {
    Dirt,
    Grass,
    Stone,
    Log,
    Sand,
    Water,
    Glass
}
impl Blocks {
    pub const fn localized_name(self) -> &'static str {
        match self {
            Blocks::Dirt  => "dirt_block",
            Blocks::Grass => "grass_block",
            Blocks::Stone => "stone_block",
            Blocks::Log   => "log_block",
            Blocks::Sand  => "sand_block",
            Blocks::Water => "water_block",
            Blocks::Glass => "glass_block",
        }
    }
}
impl AsRef<str> for Blocks {
    fn as_ref(&self) -> &str { self.localized_name() }
}

/* ---------------- mining helpers ---------------- */

#[derive(Resource, Default)]
pub struct MiningOverlayRoot(pub Option<Entity>);

#[derive(Resource, Default)]
pub struct MiningState {
    pub target: Option<MiningTarget>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MiningTarget {
    pub loc: IVec3,
    pub id: BlockId,
    pub started_at: f32,
    pub duration: f32,
}

/* ---------------- dirs/offsets ---------------- */

#[allow(dead_code)]
pub const DIR4_XZ: [IVec2; 4] = [
    IVec2::new( 1,  0),
    IVec2::new(-1,  0),
    IVec2::new( 0,  1),
    IVec2::new( 0, -1),
];

#[allow(dead_code)]
pub const DIR6: [IVec3; 6] = [
    IVec3::new( 1,  0,  0), // +X
    IVec3::new(-1,  0,  0), // -X
    IVec3::new( 0,  1,  0), // +Y
    IVec3::new( 0, -1,  0), // -Y
    IVec3::new( 0,  0,  1), // +Z
    IVec3::new( 0,  0, -1), // -Z
];

#[inline]
pub fn face_offset(f: Face) -> IVec3 {
    match f {
        Face::East   => IVec3::new( 1,  0,  0),
        Face::West   => IVec3::new(-1,  0,  0),
        Face::Top    => IVec3::new( 0,  1,  0),
        Face::Bottom => IVec3::new( 0, -1,  0),
        Face::South  => IVec3::new( 0,  0,  1),
        Face::North  => IVec3::new( 0,  0, -1),
    }
}

#[inline]
pub fn neighbor_world(wp: IVec3, f: Face) -> IVec3 { wp + face_offset(f) }

/* ---------------- space conversions ---------------- */

#[inline]
pub fn to_block_space(v: Vec3) -> Vec3 { v / VOXEL_SIZE }
#[inline]
pub fn to_world_space(v: Vec3) -> Vec3 { v * VOXEL_SIZE }

#[inline]
pub fn block_center_world(wp: IVec3) -> Vec3 {
    let s = VOXEL_SIZE;
    Vec3::new(
        (wp.x as f32 + 0.5) * s,
        (wp.y as f32 + 0.5) * s,
        (wp.z as f32 + 0.5) * s,
    )
}

#[inline]
pub fn block_origin_world(wp: IVec3) -> Vec3 {
    to_world_space(Vec3::new(wp.x as f32, wp.y as f32, wp.z as f32))
}

#[derive(Clone, Copy, Debug)]
pub struct Aabb3 { pub min: Vec3, pub max: Vec3 }

#[inline]
pub fn block_aabb_world(wp: IVec3) -> Aabb3 {
    let o = block_origin_world(wp);
    let s = VOXEL_SIZE;
    Aabb3 { min: o, max: o + Vec3::splat(s) }
}

/* ---------------- chunk lookups ---------------- */

#[inline]
pub fn chunk_and_local_from_world(wp: IVec3) -> (IVec2, usize, usize, usize) {
    let (cc, l) = world_to_chunk_xz(wp.x, wp.z);
    let lx = l.x as usize;
    let lz = l.y as usize;
    let ly = world_y_to_local(wp.y);
    (cc, lx, ly, lz)
}

#[inline]
pub fn face_visible_against(reg: &BlockRegistry, self_id: BlockId, neigh_id: BlockId) -> bool {
    if reg.is_air(self_id) { return false; }
    if reg.is_air(neigh_id) { return true; }
    if reg.is_fluid(self_id) && reg.is_fluid(neigh_id) { return false; }
    !reg.is_opaque(neigh_id)
}

#[inline]
pub fn water_face_visible_against(reg: &BlockRegistry, neigh_id: BlockId) -> bool {
    if reg.is_fluid(neigh_id) { return false; }
    !reg.is_opaque(neigh_id)
}

#[inline]
pub fn get_block_world(chunk_map: &ChunkMap, wp: IVec3) -> BlockId {
    if wp.y < Y_MIN || wp.y > Y_MAX { return 0; }
    let (cc, local) = world_to_chunk_xz(wp.x, wp.z);
    let Some(chunk) = chunk_map.chunks.get(&cc) else { return 0; };
    let lx = local.x as usize;
    let lz = local.y as usize;
    let ly = world_y_to_local(wp.y);
    chunk.get(lx, ly, lz)
}

#[inline]
pub fn get_id_world(chunk_map: &ChunkMap, wp: IVec3) -> BlockId {
    get_block_world(chunk_map, wp)
}

pub fn set_id_world(chunk_map: &mut ChunkMap, wp: IVec3, id: BlockId) -> Option<BlockId> {
    let Some(mut access) = world_access_mut(chunk_map, wp) else { return None; };
    let old = access.get();
    access.set(id);
    Some(old)
}

pub fn place_if_air(chunk_map: &mut ChunkMap, wp: IVec3, id: BlockId) -> Result<(), ()> {
    if get_id_world(chunk_map, wp) == 0 {
        set_id_world(chunk_map, wp, id);
        Ok(())
    } else {
        Err(())
    }
}

#[inline]
pub fn fluid_at_world(fluids: &FluidMap, wx: i32, wy: i32, wz: i32) -> bool {
    if wy < Y_MIN || wy > Y_MAX { return false; }
    let (cc, local) = world_to_chunk_xz(wx, wz);
    let lx = local.x as usize;
    let lz = local.y as usize;
    let ly = (wy - Y_MIN) as usize;
    match fluids.0.get(&cc) {
        Some(fc) => fc.get(lx, ly, lz),
        None => false,
    }
}

/* ---------------- misc helpers ---------------- */

#[inline]
pub fn block_name_from_registry(reg: &BlockRegistry, id: BlockId) -> String {
    reg.name_to_id
        .iter()
        .find(|&(_, &bid)| bid == id)
        .map(|(name, _)| name.clone())
        .unwrap_or_else(|| format!("#{id}"))
}

#[inline]
pub fn break_time_for(id: BlockId, registry: &BlockRegistry) -> f32 {
    let time = registry.hardness(id);
    (BASE_BREAK_TIME + PER_HARDNESS * time).clamp(MIN_BREAK_TIME, MAX_BREAK_TIME)
}

#[inline]
pub fn mining_progress(now: f32, target: &MiningTarget) -> f32 {
    ((now - target.started_at) / target.duration).clamp(0.0, 1.0)
}

/* ---------------- spawning ---------------- */

pub fn spawn_block_by_id(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    reg: &BlockRegistry,
    id: BlockId,
    world_pos: Vec3,
    size: f32,
) {
    // Uses de-duplicated mesh builder
    let mesh = build_block_cube_mesh(reg, id, size);
    commands.spawn((
        Mesh3d(meshes.add(mesh)),
        MeshMaterial3d(reg.material(id)),
        Transform::from_translation(world_pos + Vec3::splat(size * 0.5)),
        Name::new(reg.name(id).to_string()),
    ));
}

pub fn spawn_block_by_name<P: AsRef<str>>(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    reg: &BlockRegistry,
    block_ref: P,
    world_pos: Vec3,
    size: f32,
) {
    let id = reg.id(block_ref.as_ref());
    spawn_block_by_id(commands, meshes, reg, id, world_pos, size);
}

#[inline]
pub fn id_any(reg: &BlockRegistry, names: &[&str]) -> BlockId {
    for n in names {
        if let Some(&id) = reg.name_to_id.get(*n) { return id; }
    }
    0
}

/* ---------------- internal structs ---------------- */

const Z: UvRect = UvRect { u0:0.0, v0:0.0, u1:0.0, v1:0.0 };

#[derive(Deserialize)]
struct BlockTileset {
    pub image: String,
    pub tile_size: u32,
    pub columns: u32,
    pub rows: u32,
    pub tiles: HashMap<String, [u32; 2]>,
}

#[derive(Deserialize)]
struct BlockJson {
    pub name: String,
    pub texture_dir: Option<String>,
    pub texture: TextureFacesJson,
    #[serde(default)]
    pub stats: BlockStats,
}

#[derive(Deserialize)]
struct TextureFacesJson {
    // direct faces
    #[serde(default)] pub top: String,
    #[serde(default)] pub bottom: String,
    #[serde(default)] pub west: String,
    #[serde(default)] pub east: String,
    #[serde(default)] pub south: String,

    // north + legacy alias "nord"
    #[serde(default)] pub north: String,
    #[serde(default)] pub nord: String,

    // groups
    #[serde(default)] pub all: String,
    #[serde(default)] pub vertical: String,
    #[serde(default)] pub horizontal: String,
}

impl TextureFacesJson {
    fn resolve(&self) -> ResolvedFaces<'_> {
        #[inline]
        fn pick<'a>(specific: &'a str, group: &'a str, all: &'a str) -> &'a str {
            if !specific.is_empty() { specific } else if !group.is_empty() { group } else { all }
        }

        let north_name = if !self.north.is_empty() { self.north.as_str() } else { self.nord.as_str() };

        ResolvedFaces {
            top:    pick(&self.top,    &self.vertical,   &self.all),
            bottom: pick(&self.bottom, &self.vertical,   &self.all),
            north:  pick(north_name,   &self.horizontal, &self.all),
            east:   pick(&self.east,   &self.horizontal, &self.all),
            south:  pick(&self.south,  &self.horizontal, &self.all),
            west:   pick(&self.west,   &self.horizontal, &self.all),
        }
    }
}

struct ResolvedFaces<'a> {
    top:   &'a str,
    bottom:&'a str,
    north: &'a str,
    east:  &'a str,
    south: &'a str,
    west:  &'a str,
}

/* ---------------- defaults + io ---------------- */

fn d_true() -> bool { true }

#[inline]
fn require_face<'a>(name: &'a str, face: &str, block_name: &str) -> &'a str {
    if name.is_empty() {
        panic!(
            "block '{}': missing texture for face '{}'. Provide '{}' or use 'all'/'vertical'/'horizontal'.",
            block_name, face, face
        );
    }
    name
}

fn read_json<T: for<'de> Deserialize<'de>>(path: &str) -> T {
    let s = fs::read_to_string(path).unwrap_or_else(|_| panic!("missing file: {path}"));
    serde_json::from_str(&s).unwrap_or_else(|e| panic!("invalid JSON '{path}': {e}"))
}

fn guess_tex_dir_from_block_name(block_name: &str) -> String {
    let base = block_name.strip_suffix("_block").unwrap_or(block_name);
    format!("textures/blocks/{}", base)
}

/* ---------------- uv helpers ---------------- */

fn tile_uv(ts: &BlockTileset, name: &str) -> Result<UvRect, String> {
    let [col, row] = *ts.tiles.get(name)
        .ok_or_else(|| format!("tile '{}' not in data.json", name))?;

    if col >= ts.columns || row >= ts.rows {
        return Err(format!("tile '{}' out of bounds ({}x{})", name, ts.columns, ts.rows));
    }

    let img_w = (ts.columns * ts.tile_size) as f32;
    let img_h = (ts.rows * ts.tile_size) as f32;

    let ([u0, v0], [u1, v1]) = atlas_uv(
        col as usize,
        row as usize,
        ts.columns as usize,
        ts.rows as usize,
        ATLAS_PAD_PX,
        img_w,
        img_h,
    );

    Ok(UvRect { u0, v0, u1, v1 })
}

fn atlas_uv(tile_x: usize, tile_y: usize, tiles_x: usize, tiles_y: usize,
            pad_px: f32, image_w: f32, image_h: f32) -> ([f32;2],[f32;2]) {
    let tw = image_w / tiles_x as f32;
    let th = image_h / tiles_y as f32;

    let u0 = (tile_x as f32 * tw + pad_px) / image_w;
    let v0 = (tile_y as f32 * th + pad_px) / image_h;
    let u1 = ((tile_x as f32 + 1.0) * tw - pad_px) / image_w;
    let v1 = ((tile_y as f32 + 1.0) * th - pad_px) / image_h;

    ([u0, v0], [u1, v1])
}

/* ---------------- cube mesh builder (de-duplicated) ---------------- */

/// Build a cube mesh from per-face UVs given as a tuple.
/// Order: (Top, Bottom, North, East, South, West).
pub fn cube_mesh_from_faces_tuple(
    faces: (UvRect, UvRect, UvRect, UvRect, UvRect, UvRect),
    size: f32,
) -> Mesh {
    #[inline]
    fn quad_uv(uv: &UvRect, flip_v: bool) -> [[f32; 2]; 4] {
        if !flip_v {
            [[uv.u0, uv.v0], [uv.u1, uv.v0], [uv.u1, uv.v1], [uv.u0, uv.v1]]
        } else {
            [[uv.u0, uv.v1], [uv.u1, uv.v1], [uv.u1, uv.v0], [uv.u0, uv.v0]]
        }
    }

    let (top, bottom, north, east, south, west) = faces;
    let s = size;

    let mut pos: Vec<[f32; 3]> = Vec::with_capacity(24);
    let mut nrm: Vec<[f32; 3]> = Vec::with_capacity(24);
    let mut uvs: Vec<[f32; 2]> = Vec::with_capacity(24);
    let mut idx: Vec<u32>      = Vec::with_capacity(36);

    #[inline]
    fn append_quad(
        pos: &mut Vec<[f32; 3]>,
        nrm: &mut Vec<[f32; 3]>,
        uvs: &mut Vec<[f32; 2]>,
        idx: &mut Vec<u32>,
        quad: [[f32; 3]; 4],
        normal: [f32; 3],
        uv: &UvRect,
        flip_v: bool,
    ) {
        let base = pos.len() as u32;
        pos.extend_from_slice(&quad);
        nrm.extend_from_slice(&[normal; 4]);
        uvs.extend_from_slice(&quad_uv(uv, flip_v));
        idx.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 3]);
    }

    // +X (East)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[s,0.0,s],[s,0.0,0.0],[s,s,0.0],[s,s,s]],
        [1.0,0.0,0.0], &east, true
    );
    // -X (West)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[0.0,0.0,0.0],[0.0,0.0,s],[0.0,s,s],[0.0,s,0.0]],
        [-1.0,0.0,0.0], &west, true
    );
    // +Y (Top)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[0.0,s,s],[s,s,s],[s,s,0.0],[0.0,s,0.0]],
        [0.0,1.0,0.0], &top, false
    );
    // -Y (Bottom)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[0.0,0.0,0.0],[s,0.0,0.0],[s,0.0,s],[0.0,0.0,s]],
        [0.0,-1.0,0.0], &bottom, false
    );
    // +Z (South)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[0.0,0.0,s],[s,0.0,s],[s,s,s],[0.0,s,s]],
        [0.0,0.0,1.0], &south, true
    );
    // -Z (North)
    append_quad(
        &mut pos, &mut nrm, &mut uvs, &mut idx,
        [[s,0.0,0.0],[0.0,0.0,0.0],[0.0,s,0.0],[s,s,0.0]],
        [0.0,0.0,-1.0], &north, true
    );

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   nrm);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0,     uvs);
    mesh.insert_indices(Indices::U32(idx));
    mesh
}

/// Convenience: build a cube mesh for a given block id using the registry UVs.
pub fn build_block_cube_mesh(reg: &BlockRegistry, id: BlockId, size: f32) -> Mesh {
    cube_mesh_from_faces_tuple(reg.face_uvs(id), size)
}

/* ---------------- small utils ---------------- */

#[inline] fn world_y_to_local(wy: i32) -> usize { (wy - Y_MIN) as usize }

#[inline]
fn material_policy_from_stats(stats: &BlockStats) -> (AlphaMode, Color) {
    if stats.opaque {
        (AlphaMode::Opaque, Color::WHITE)
    } else {
        (AlphaMode::Blend, Color::srgba(1.0, 1.0, 1.0, 0.8))
    }
}
