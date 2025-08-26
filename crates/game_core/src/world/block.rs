use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use serde::Deserialize;
use std::collections::HashMap;
// =================================================================
//                          External Struct
// =================================================================

#[derive(Deserialize)]
pub struct Tileset {
    pub image: String,
    pub tile_size: u32,
    pub columns: u32,
    pub rows: u32,
    pub tiles: HashMap<String, [u32; 2]>,
}

#[derive(Deserialize)]
pub struct BlockJson {
    pub name: String,
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

pub fn spawn_block_from_files(
    commands: &mut Commands,
    asset_server: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    tileset_json_path: &str,
    block_json_path: &str,
    world_position: Vec3,
    size: f32,
) {
    let tileset_str = std::fs::read_to_string(format!("assets/{tileset_json_path}"))
        .expect("tileset.json not found (put them ito assets/)");
    let tileset: Tileset = serde_json::from_str(&tileset_str).expect("tileset.json invalid");

    let block_str = std::fs::read_to_string(format!("assets/{block_json_path}"))
        .expect("block json not found (put them ito assets/)");
    let block_json: BlockJson = serde_json::from_str(&block_str).expect("block json invalid");

    let face_uvs = faces_to_uvs(&tileset, &block_json.texture)
        .unwrap_or_else(|e| panic!("UV mapping failure: {e}"));

    let mesh = cube_mesh_with_face_uvs(&face_uvs, size);

    let atlas_image: Handle<Image> = asset_server.load(tileset.image.as_str());
    let material = StandardMaterial {
        base_color_texture: Some(atlas_image.clone()),
        // unlit: true, // <- lower light
        ..Default::default()
    };

    commands.spawn((
        Mesh3d(meshes.add(mesh)),
        MeshMaterial3d(materials.add(material)),
        Transform::from_translation(world_position + Vec3::splat(size * 0.5)),
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

fn faces_to_uvs(tileset: &Tileset, faces: &TextureFacesJson) -> Result<FaceUvRects, String> {
    let north_name = if !faces.north.is_empty() {
        faces.north.clone()
    } else if !faces.nord.is_empty() {
        faces.nord.clone()
    } else {
        return Err("No north face".to_string());
    };

    Ok(FaceUvRects {
        top: tile_uv(tileset, &faces.top)?,
        bottom: tile_uv(tileset, &faces.bottom)?,
        west: tile_uv(tileset, &faces.west)?,
        east: tile_uv(tileset, &faces.east)?,
        south: tile_uv(tileset, &faces.south)?,
        north: tile_uv(tileset, &north_name)?
    })
}

fn tile_uv(tileset: &Tileset, name: &str) -> Result<UvRect, String> {
    let [col, row] = *tileset.tiles.get(name)
        .ok_or_else(|| format!("Tile '{}' not in Tileset", name))?;
    if col >= tileset.columns || row >= tileset.rows {
        return Err(format!("Tile '{}' out of bounds of atlas", name));
    }

    let du = 1.0 / tileset.columns as f32;
    let dv = 1.0 / tileset.rows as f32;

    let mut u0 = col as f32 * du;
    let mut v0 = row as f32 * dv;
    let mut u1 = u0 + du;
    let mut v1 = v0 + dv;

    let atlas_w = (tileset.columns * tileset.tile_size) as f32;
    let atlas_h = (tileset.rows * tileset.tile_size) as f32;
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

    let mut positions = Vec::with_capacity(24);
    let mut normals   = Vec::with_capacity(24);
    let mut uvs       = Vec::with_capacity(24);
    let mut indices   = Vec::with_capacity(36);

    let mut push_face = |quad: [[f32; 3]; 4], normal: [f32; 3], uv: &UvRect, flip_v: bool| {
        let base = positions.len() as u32;
        positions.extend_from_slice(&quad);
        normals.extend_from_slice(&[normal; 4]);
        uvs.extend_from_slice(&quad_uv(uv, flip_v));
        indices.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 3]);
    };

    push_face([[s,0.0,s],[s,0.0,0.0],[s,s,0.0],[s,s,s]],[1.0,0.0,0.0], &f.east,  true);

    push_face([[0.0,0.0,0.0],[0.0,0.0,s],[0.0,s,s],[0.0,s,0.0]],[-1.0,0.0,0.0], &f.west,  true);

    push_face([[0.0,s,s],[s,s,s],[s,s,0.0],[0.0,s,0.0]],[0.0,1.0,0.0],           &f.top,   false);

    push_face([[0.0,0.0,0.0],[s,0.0,0.0],[s,0.0,s],[0.0,0.0,s]],[0.0,-1.0,0.0],  &f.bottom,false);

    push_face([[0.0,0.0,s],[s,0.0,s],[s,s,s],[0.0,s,s]],[0.0,0.0,1.0],           &f.south, true);

    push_face([[s,0.0,0.0],[0.0,0.0,0.0],[0.0,s,0.0],[s,s,0.0]],[0.0,0.0,-1.0],  &f.north, true);

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(Indices::U32(indices));
    mesh
}