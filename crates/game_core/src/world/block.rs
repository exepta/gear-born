use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
// =================================================================
//                          External Struct
// =================================================================

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Blocks {
    Dirt,
    Grass
}

impl Blocks {
    pub const fn json_path(self) -> &'static str {
        match self {
            Blocks::Dirt  => "blocks/dirt_block.json",
            Blocks::Grass => "blocks/grass_block.json",
        }
    }
}

impl AsRef<str> for Blocks {
    fn as_ref(&self) -> &str {
        self.json_path()
    }
}

#[derive(Deserialize)]
pub struct BlockTileset {
    pub image: String,
    pub tile_size: u32,
    pub columns: u32,
    pub rows: u32,
    pub tiles: HashMap<String, [u32; 2]>,
}

#[derive(Deserialize)]
pub struct BlockJson {
    pub name: String,
    pub texture_dir: Option<String>,
    pub texture: TextureFacesJson,
    #[serde(default)]
    pub stats: BlockStats,
}

#[derive(Deserialize)]
pub struct TextureFacesJson {
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

#[derive(Deserialize, Clone, Default)]
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

fn d_true() -> bool { true }

// =================================================================
//                        External Function
// =================================================================

pub fn spawn_block_from_file<P: AsRef<str>>(
    commands: &mut Commands,
    asset_server: &AssetServer,
    meshes: &mut Assets<Mesh>,
    materials: &mut Assets<StandardMaterial>,
    block_ref: P,
    world_pos: Vec3,
    size: f32,
) {
    let block_json_path: &str = block_ref.as_ref();
    let block_json: BlockJson = read_json(&format!("assets/{block_json_path}"));
    let tex_dir = block_json
        .texture_dir
        .clone()
        .unwrap_or_else(|| guess_tex_dir_from_block_name(&block_json.name));
    let tileset_path = format!("assets/{}/data.json", tex_dir);
    let tileset: BlockTileset = read_json(&tileset_path);

    let atlas_path = format!("{}/{}", tex_dir, tileset.image);
    let atlas_image: Handle<Image> = asset_server.load(atlas_path.as_str());

    // UVs je Face aus dem per-Block-Tileset
    let face_uvs = collect_face_uvs(&tileset, &block_json.texture)
        .unwrap_or_else(|e| panic!("UV mapping failed for '{}': {e}", block_json.name));

    // Mesh + Material
    let mesh = cube_mesh_with_face_uvs(&face_uvs, size);
    let material = StandardMaterial {
        base_color_texture: Some(atlas_image),
        ..Default::default()
    };

    commands.spawn((
        Mesh3d(meshes.add(mesh)),
        MeshMaterial3d(materials.add(material)),
        Transform::from_translation(world_pos + Vec3::splat(size * 0.5)),
        Name::new(block_json.name),
    ));
}

// =================================================================
//                          Internal Struct
// =================================================================

#[derive(Clone)]
struct UvRect { u0: f32, v0: f32, u1: f32, v1: f32 }

#[derive(Clone)]
struct FaceUvRects {
    top: UvRect,
    bottom: UvRect,
    north: UvRect,
    east: UvRect,
    west: UvRect,
    south: UvRect,
}

// =================================================================
//                        Internal Function
// =================================================================

fn read_json<T: for<'de> Deserialize<'de>>(path: &str) -> T {
    let s = fs::read_to_string(path).unwrap_or_else(|_| panic!("missing file: {path}"));
    serde_json::from_str(&s).unwrap_or_else(|e| panic!("invalid JSON '{path}': {e}"))
}

fn guess_tex_dir_from_block_name(block_name: &str) -> String {
    let base = block_name.strip_suffix("_block").unwrap_or(block_name);
    format!("textures/blocks/{}", base)
}

fn collect_face_uvs(ts: &BlockTileset, faces: &TextureFacesJson) -> Result<FaceUvRects, String> {
    let north_name = if !faces.north.is_empty() { &faces.north }
    else if !faces.nord.is_empty() { &faces.nord }
    else { return Err("north/nord missing".into()); };

    Ok(FaceUvRects {
        top:    tile_uv(ts, &faces.top)?,
        bottom: tile_uv(ts, &faces.bottom)?,
        west:   tile_uv(ts, &faces.west)?,
        east:   tile_uv(ts, &faces.east)?,
        south:  tile_uv(ts, &faces.south)?,
        north:  tile_uv(ts, north_name)?,
    })
}

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