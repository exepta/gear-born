use crate::world::chunk::ChunkMap;
use crate::world::chunk_dim::{world_to_chunk_xz, Y_MAX, Y_MIN};
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub const VOXEL_SIZE: f32 = 1.5;

// =================================================================
//                          External Struct
// =================================================================

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
    pub emissive: f32,
}

#[derive(Resource)]
pub struct BlockRegistry {
    pub defs: Vec<BlockDef>,
    pub name_to_id: HashMap<String, BlockId>,
}

impl BlockRegistry {

    pub fn material(&self, id: BlockId) -> Handle<StandardMaterial> {
        self.def(id).material.clone()
    }

    pub fn load_all(
        asset_server: &AssetServer,
        materials: &mut Assets<StandardMaterial>,
        blocks_dir: &str, // z.B. "assets/blocks"
    ) -> Self {
        // Reserve 0 = Air
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
            let tileset_path = format!("assets/{}/data.json", tex_dir);
            let tileset: BlockTileset = read_json(&tileset_path);

            let atlas_path = format!("{}/{}", tex_dir, tileset.image);
            let image: Handle<Image> = asset_server.load(atlas_path.as_str());

            // UVs je Face
            let uv_top    = tile_uv(&tileset, &block_json.texture.top).unwrap();
            let uv_bottom = tile_uv(&tileset, &block_json.texture.bottom).unwrap();
            let uv_west   = tile_uv(&tileset, &block_json.texture.west).unwrap();
            let uv_east   = tile_uv(&tileset, &block_json.texture.east).unwrap();
            let uv_south  = tile_uv(&tileset, &block_json.texture.south).unwrap();
            let north_key = if !block_json.texture.north.is_empty() {
                &block_json.texture.north
            } else {
                &block_json.texture.nord
            };
            let uv_north  = tile_uv(&tileset, north_key).unwrap();

            let alpha_mode = if block_json.stats.opaque {
                AlphaMode::Opaque
            } else {
                AlphaMode::Blend
            };

            let base_color = if block_json.stats.opaque {
                Color::WHITE
            } else {
                Color::srgba(1.0, 1.0, 1.0, 0.8)
            };

            let material = materials.add(StandardMaterial {
                base_color_texture: Some(image.clone()),
                base_color,
                alpha_mode,
                unlit: false,
                metallic: 0.0,
                perceptual_roughness: 1.0,
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

    #[inline] pub fn id(&self, name:&str) -> BlockId {
        *self.name_to_id.get(name).expect("unknown block name")
    }

    #[inline] pub fn def(&self, id: BlockId) -> &BlockDef { &self.defs[id as usize] }

    #[inline] pub fn uv(&self, id: BlockId, face: Face) -> UvRect {
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
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Blocks {
    Dirt,
    Grass,
    Stone,
    Log,
    Sand,
    Water
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
        }
    }
}

impl AsRef<str> for Blocks {
    fn as_ref(&self) -> &str {
        self.localized_name()
    }
}

// =================================================================
//                        External Function
// =================================================================

pub fn spawn_block_by_id(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    reg: &BlockRegistry,
    id: BlockId,
    world_pos: Vec3,
    size: f32,
) {
    let def = reg.def(id);
    let faces = FaceUvRects {
        top: def.uv_top,
        bottom: def.uv_bottom,
        north: def.uv_north,
        east: def.uv_east,
        south: def.uv_south,
        west: def.uv_west,
    };
    let mesh = cube_mesh_with_face_uvs(&faces, size);

    commands.spawn((
        Mesh3d(meshes.add(mesh)),
        MeshMaterial3d(def.material.clone()),
        Transform::from_translation(world_pos + Vec3::splat(size * 0.5)),
        Name::new(def.name.clone()),
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

pub fn id_any(reg: &BlockRegistry, names: &[&str]) -> BlockId {
    for n in names {
        if let Some(&id) = reg.name_to_id.get(*n) { return id; }
    }
    0
}

#[inline] pub fn to_block_space(v: Vec3) -> Vec3 { v / VOXEL_SIZE }
#[inline] pub fn to_world_space(v: Vec3) -> Vec3 { v * VOXEL_SIZE }

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
pub fn block_name_from_registry(reg: &BlockRegistry, id: BlockId) -> String {
    reg.name_to_id
        .iter()
        .find(|&(_, &bid)| bid == id)
        .map(|(name, _)| name.clone())
        .unwrap_or_else(|| format!("#{id}"))
}

// =================================================================
//                          Internal Struct
// =================================================================

const Z: UvRect = UvRect { u0:0.0, v0:0.0, u1:0.0, v1:0.0 };

#[derive(Clone)]
struct FaceUvRects {
    top: UvRect,
    bottom: UvRect,
    north: UvRect,
    east: UvRect,
    west: UvRect,
    south: UvRect,
}

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
    pub top: String,
    pub bottom: String,
    pub west: String,
    #[serde(default)]
    pub nord: String,
    #[serde(default)]
    pub north: String,
    pub east: String,
    pub south: String,
}

fn d_true() -> bool { true }

// =================================================================
//                        Internal Function
// =================================================================

/// Reads a UTF-8 JSON file from `path` and deserializes it into `T`.
///
/// The generic type `T` must implement `Deserialize`.
///
/// # Parameters
/// - `path`: Filesystem path to the JSON file.
///
/// # Returns
/// A value of type `T` parsed from the file contents.
///
/// # Panics
/// - If the file cannot be read, with a message `missing file: {path}`.
/// - If the file contents are not valid JSON for `T`, with a message `invalid JSON '{path}': {error}`.
fn read_json<T: for<'de> Deserialize<'de>>(path: &str) -> T {
    let s = fs::read_to_string(path).unwrap_or_else(|_| panic!("missing file: {path}"));
    serde_json::from_str(&s).unwrap_or_else(|e| panic!("invalid JSON '{path}': {e}"))
}

/// Guesses a texture directory path for a block by its identifier.
///
/// If `block_name` ends with `"_block"`, that suffix is stripped; otherwise the full name is used.
/// The returned path is of the form: `textures/blocks/{base}`.
///
/// # Parameters
/// - `block_name`: Logical block identifier, optionally ending with `"_block"`.
///
/// # Returns
/// A relative texture directory path for the block.
fn guess_tex_dir_from_block_name(block_name: &str) -> String {
    let base = block_name.strip_suffix("_block").unwrap_or(block_name);
    format!("textures/blocks/{}", base)
}

/// Computes a normalized UV rectangle for a named tile within a tileset atlas.
///
/// The UV rectangle is in the `[0, 1] × [0, 1]` range. A half-texel inset is applied
/// to all sides to reduce sampling bleeding between adjacent tiles.
///
/// # Parameters
/// - `ts`: Tileset containing the atlas dimensions, tile size, and tile name mapping.
/// - `name`: Tile identifier to resolve within the tileset.
///
/// # Returns
/// A `UvRect { u0, v0, u1, v1 }` spanning the tile in normalized coordinates.
///
/// # Errors
/// - If `name` is not found in the tileset mapping.
/// - If the resolved tile coordinates exceed the tileset bounds.
fn tile_uv(ts: &BlockTileset, name: &str) -> Result<UvRect, String> {
    let [col, row] = *ts.tiles.get(name)
        .ok_or_else(|| format!("tile '{}' not in data.json", name))?;

    if col >= ts.columns || row >= ts.rows {
        return Err(format!("tile '{}' out of bounds ({}x{})", name, ts.columns, ts.rows));
    }

    let du = 1.0 / ts.columns as f32;
    let dv = 1.0 / ts.rows as f32;
    let mut u0 = col as f32 * du;
    let mut v0 = row as f32 * dv;
    let mut u1 = u0 + du;
    let mut v1 = v0 + dv;

    // half-texel inset
    let atlas_w = (ts.columns * ts.tile_size) as f32;
    let atlas_h = (ts.rows * ts.tile_size) as f32;
    let eps_u = 0.5 / atlas_w;
    let eps_v = 0.5 / atlas_h;
    u0 += eps_u; v0 += eps_v; u1 -= eps_u; v1 -= eps_v;

    Ok(UvRect { u0, v0, u1, v1 })
}

/// Builds a cube mesh with per-face UVs and outward-facing normals.
///
/// Generates a cube spanning the range `[0, size]` on each axis. Each face
/// uses its own four vertices (no shared vertices across faces) to allow
/// distinct UV mapping. Triangle winding is counter-clockwise for outward
/// normals. UV orientation per face may flip `v` to match the expected
/// texture orientation.
///
/// The resulting `Mesh` contains:
/// - `Mesh::ATTRIBUTE_POSITION` (24 vertices),
/// - `Mesh::ATTRIBUTE_NORMAL` (per-vertex outward axis-aligned normals),
/// - `Mesh::ATTRIBUTE_UV_0` (per-vertex UVs),
/// and indexed triangles (`u32`) with `PrimitiveTopology::TriangleList`.
///
/// # Parameters
/// - `f`: UV rectangles for each cube face.
/// - `size`: Edge length of the cube.
///
/// # Returns
/// A `Mesh` ready to be rendered, with positions, normals, UVs, and indices populated.
///
/// # Notes
/// The cube is not centered at the origin; it occupies `[0, size]` in X/Y/Z.
fn cube_mesh_with_face_uvs(f: &FaceUvRects, size: f32) -> Mesh {
    let s = size;

    #[inline]
    fn quad_uv(uv: &UvRect, flip_v: bool) -> [[f32; 2]; 4] {
        if !flip_v {
            [[uv.u0, uv.v0], [uv.u1, uv.v0], [uv.u1, uv.v1], [uv.u0, uv.v1]]
        } else {
            [[uv.u0, uv.v1], [uv.u1, uv.v1], [uv.u1, uv.v0], [uv.u0, uv.v0]]
        }
    }

    let mut pos = Vec::with_capacity(24);
    let mut nrm = Vec::with_capacity(24);
    let mut uvs = Vec::with_capacity(24);
    let mut idx = Vec::with_capacity(36);

    let mut push = |quad: [[f32; 3]; 4], normal: [f32; 3], uv: &UvRect, flip_v: bool| {
        let base = pos.len() as u32;
        pos.extend_from_slice(&quad);
        nrm.extend_from_slice(&[normal; 4]);
        uvs.extend_from_slice(&quad_uv(uv, flip_v));
        idx.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 3]);
    };

    // East (+X)  — CCW
    push([[s,0.0,s],[s,0.0,0.0],[s,s,0.0],[s,s,s]],[1.0,0.0,0.0], &f.east,  true);
    // West (-X)  — CCW
    push([[0.0,0.0,0.0],[0.0,0.0,s],[0.0,s,s],[0.0,s,0.0]],[-1.0,0.0,0.0], &f.west,  true);
    // Top (+Y)
    push([[0.0,s,s],[s,s,s],[s,s,0.0],[0.0,s,0.0]],[0.0,1.0,0.0],           &f.top,   false);
    // Bottom (-Y)
    push([[0.0,0.0,0.0],[s,0.0,0.0],[s,0.0,s],[0.0,0.0,s]],[0.0,-1.0,0.0],  &f.bottom,false);
    // South (+Z)
    push([[0.0,0.0,s],[s,0.0,s],[s,s,s],[0.0,s,s]],[0.0,0.0,1.0],           &f.south, true);
    // North (-Z)
    push([[s,0.0,0.0],[0.0,0.0,0.0],[0.0,s,0.0],[s,s,0.0]],[0.0,0.0,-1.0],  &f.north, true);

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, pos);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL,   nrm);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0,     uvs);
    mesh.insert_indices(Indices::U32(idx));
    mesh
}

#[inline] fn world_y_to_local(wy: i32) -> usize { (wy - Y_MIN) as usize }