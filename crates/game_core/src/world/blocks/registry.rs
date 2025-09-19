use std::collections::HashMap;
use std::fs;
use std::path::Path;
use bevy::prelude::*;
use serde::Deserialize;
use crate::config::TextureQuality;
use crate::world::blocks::{BlockDefinition, BlockForm, BlockId, BlockStats, BlockUvs, Face, UvRect};

#[derive(Resource, Clone)]
pub struct BlockRegistry {
    pub blocks: Vec<BlockDefinition>,
    pub block_ids: HashMap<String, BlockId>
}

impl BlockRegistry {

    pub fn definition(&self, id: BlockId) -> &BlockDefinition { &self.blocks[id as usize] }

    pub fn name(&self, id: BlockId) -> &str { &self.blocks[id as usize].name.as_str() }

    pub fn name_optional(&self, id: BlockId) -> Option<&str> {
        self.blocks.get(id as usize).map(|b| b.name.as_str())
    }

    pub fn id(&self, name: &str) -> BlockId {
        self.block_ids[name]
    }

    pub fn id_optional(&self, name: &str) -> Option<BlockId> {
        self.block_ids.get(name).copied()
    }

    pub fn id_or_default(&self, name: &str) -> BlockId {
        self.id_optional(name).unwrap_or(0)
    }

    pub fn material(&self, id: BlockId) -> Handle<StandardMaterial> {
        self.definition(id).material.clone()
    }

    pub fn stats(&self, id: BlockId) -> &BlockStats { &self.definition(id).stats }

    pub fn is_air(&self, id: BlockId) -> bool { id == 0 }

    pub fn is_solid(&self, id: BlockId) -> bool {
        self.definition(id).stats.block_form.eq(&BlockForm::Solid)
    }

    pub fn is_fluid(&self, id: BlockId) -> bool {
        self.definition(id).stats.block_form.eq(&BlockForm::Fluid)
    }

    pub fn is_opaque(&self, id: BlockId) -> bool {
        self.stats(id).opaque
    }

    pub fn emissive(&self, id: BlockId) -> f32 {
        self.stats(id).emissive
    }

    pub fn hardness(&self, id: BlockId) -> f32 {
        self.stats(id).hardness
    }

    pub fn hardness_level(&self, id: BlockId) -> u8 {
        self.stats(id).hardness_level
    }

    pub fn blast_resistance(&self, id: BlockId) -> f32 {
        self.stats(id).blast_resistance
    }

    pub fn texture(&self, id: BlockId) -> Handle<Image> {
        self.definition(id).texture.clone()
    }

    pub fn uv_from_face(&self, id: BlockId, face: Face) -> UvRect {
        let uvs = self.definition(id).uvs;
        match face {
            Face::Top => uvs.top,
            Face::Bottom => uvs.bottom,
            Face::North => uvs.north,
            Face::East => uvs.east,
            Face::South => uvs.south,
            Face::West => uvs.west
        }
    }

    pub fn face_uvs(&self, id: BlockId) -> (UvRect, UvRect, UvRect, UvRect, UvRect, UvRect) {
        let uvs = self.definition(id).uvs;
        (uvs.top, uvs.bottom, uvs.north, uvs.east, uvs.south, uvs.west)
    }
    
    pub fn load_all_with_resources(
        asset_server: &AssetServer,
        materials: &mut Assets<StandardMaterial>,
        blocks_folder: &str,
        texture_quality: TextureQuality
    ) -> Self {
        let mut definitions: Vec<BlockDefinition> = Vec::new();
        let mut block_ids: HashMap<String, BlockId> = HashMap::new();
        
        definitions.push(BlockDefinition {
            name: String::from("air"),
            stats: BlockStats::default(),
            uvs: BlockUvs::default(),
            texture: Handle::default(),
            material: Handle::default()
        });
        
        block_ids.insert(String::from("air"), 0);
        
        let folder = Path::new(blocks_folder);
        for entry in fs::read_dir(folder).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().and_then(|s| s.to_str()) != Some("json") { continue; }

            let block_json: BlockJson = read_json(path.to_str().unwrap());
            let tex_dir = block_json
                .texture_dir
                .clone()
                .unwrap_or_else(|| guess_tex_dir_from_block_name(&block_json.name));

            let tileset_path = format!("assets/{}/data.json", tex_dir);
            let mut tileset: BlockTileset = read_json(&tileset_path);

            let desired_px = texture_quality.clone().tile_px();
            let (atlas_file, effective_px) = resolve_atlas_for_res(&tex_dir, &tileset.image, desired_px);

            tileset.tile_size = effective_px;
            let atlas_path = format!("{}/{}", tex_dir, atlas_file);
            let image: Handle<Image> = asset_server.load(atlas_path.as_str());

            let faces = block_json.texture.resolve();
            let top    = tile_uv(&tileset, require_face(&faces.top,    "top",    &block_json.name)).unwrap();
            let bottom = tile_uv(&tileset, require_face(&faces.bottom, "bottom", &block_json.name)).unwrap();
            let north  = tile_uv(&tileset, require_face(&faces.north,  "north",  &block_json.name)).unwrap();
            let east   = tile_uv(&tileset, require_face(&faces.east,   "east",   &block_json.name)).unwrap();
            let south  = tile_uv(&tileset, require_face(&faces.south,  "south",  &block_json.name)).unwrap();
            let west   = tile_uv(&tileset, require_face(&faces.west,   "west",   &block_json.name)).unwrap();

            let (alpha_mode, base_color) = material_policy_from_stats(&block_json.stats);
            
            let material = materials.add(StandardMaterial {
                base_color_texture: Some(image.clone()),
                base_color,
                alpha_mode,
                unlit: false,
                metallic: 0.0,
                perceptual_roughness: 1.0,
                reflectance: 0.0,
                ..default()
            });
            
            let id = definitions.len() as BlockId;
            block_ids.insert(block_json.name.clone(), id);
            definitions.push(BlockDefinition {
                name: block_json.name,
                stats: block_json.stats,
                uvs: BlockUvs { top, bottom, east, south, west, north },
                texture: image,
                material
            });
        }
        
        Self {
            blocks: definitions,
            block_ids
        }
    }

}

// =================================================================================================
//
//                                            Json Mapper
//
// =================================================================================================

#[derive(Deserialize)]
struct BlockTileset {
    pub image: String,
    pub tile_size: u16,
    pub columns: u16,
    pub rows: u16,
    pub tiles: HashMap<String, [u16; 2]>,
}

struct ResolvedFaces<'a> {
    top:   &'a str,
    bottom:&'a str,
    north: &'a str,
    east:  &'a str,
    south: &'a str,
    west:  &'a str,
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

#[derive(Deserialize)]
struct BlockJson {
    pub name: String,
    pub texture_dir: Option<String>,
    pub texture: TextureFacesJson,
    #[serde(default)]
    pub stats: BlockStats,
}

fn read_json<T: for<'de> Deserialize<'de>>(path: &str) -> T {
    let s = fs::read_to_string(path).unwrap_or_else(|_| panic!("missing file: {path}"));
    serde_json::from_str(&s).unwrap_or_else(|e| panic!("invalid JSON '{path}': {e}"))
}

fn guess_tex_dir_from_block_name(block_name: &str) -> String {
    let base = block_name.strip_suffix("_block").unwrap_or(block_name);
    format!("textures/blocks/{}", base)
}

fn resolve_atlas_for_res(tex_dir: &str, base_image: &str, desired_px: u16) -> (String, u16) {
    // Low res requested: nothing to resolve.
    if desired_px == 64 {
        return (base_image.to_string(), 64);
    }

    // Construct candidate like "border_128.png".
    let candidate = image_name_for_res(base_image, desired_px);

    // Check physical file existence: assets/<tex_dir>/<candidate>
    let full_path = Path::new("assets").join(tex_dir).join(&candidate);
    if full_path.exists() {
        (candidate, desired_px)
    } else {
        // Graceful per-block fallback
        warn!(
            "Missing atlas: {:?}. Falling back to 64px for this block ({}).",
            full_path, base_image
        );
        (base_image.to_string(), 64)
    }
}

fn image_name_for_res(base_name: &str, tile_px: u16) -> String {
    if tile_px == 64 {
        return base_name.to_string();
    }
    let p = Path::new(base_name);
    let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or(base_name);
    let ext = p.extension().and_then(|s| s.to_str()).unwrap_or("png");
    format!("{stem}_{tile_px}.{ext}")
}

fn tile_uv(ts: &BlockTileset, name: &str) -> Result<UvRect, String> {
    let [col, row] = *ts.tiles.get(name)
        .ok_or_else(|| format!("tile '{}' not in data.json", name))?;

    if col >= ts.columns || row >= ts.rows {
        return Err(format!("tile '{}' out of bounds ({}x{})", name, ts.columns, ts.rows));
    }

    let img_w = (ts.columns * ts.tile_size) as f32;
    let img_h = (ts.rows * ts.tile_size) as f32;

    let ([left, top], [right, bottom]) = atlas_uv(
        col as usize,
        row as usize,
        ts.columns as usize,
        ts.rows as usize,
        0.5, // Atlas pad px
        img_w,
        img_h,
    );

    Ok(UvRect { left, top, right, bottom })
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

fn material_policy_from_stats(stats: &BlockStats) -> (AlphaMode, Color) {
    if stats.opaque {
        (AlphaMode::Opaque, Color::WHITE)
    } else {
        (AlphaMode::Blend, Color::srgba(1.0, 1.0, 1.0, 0.8))
    }
}
