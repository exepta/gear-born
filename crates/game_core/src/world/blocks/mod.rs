pub mod registry;

use bevy::prelude::*;
use serde::Deserialize;
use crate::serde_defaults::default_true;

/// This is the identifier for a block. This can be used to identify a block in a world.
/// U can use [`u16`] instance of [`BlockId`]. Note that the block id can't be higher than (65.535)
/// The id is used in all relevant logic and systems.
pub type BlockId = u16;

/// Block faces for the direction of a block. This is used for the mesher and
/// if the user looks at a face.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Face { Top, Bottom, North, East, South, West }

/// A rectangle in UV space. This is used in GL space.
/// This means 0.0 to 1.0 like old OpenGL systems.
#[derive(Clone, Copy, Debug)]
pub struct UvRect { pub left:f32, pub top:f32, pub right:f32, pub bottom:f32 }

impl UvRect {

    pub fn zero() -> Self {
        Self {
            left: 0.0,
            top: 0.0,
            right: 0.0,
            bottom: 0.0
        }
    }

}

/// A block's UV coordinates.
#[derive(Clone, Copy, Debug)]
pub struct BlockUvs {
    pub top: UvRect,
    pub bottom: UvRect,
    pub north: UvRect,
    pub south: UvRect,
    pub east: UvRect,
    pub west: UvRect
}

impl Default for BlockUvs {
    fn default() -> Self {
        Self {
            north: UvRect::zero(),
            bottom: UvRect::zero(),
            east: UvRect::zero(),
            south: UvRect::zero(),
            west: UvRect::zero(),
            top: UvRect::zero()
        }
    }
}

/// Block states for a handle in code. Currently used:
/// - [`hardness`] higher value means the block needs more time for break
/// - [`hardness_level`] level for tools like shovels, axes, etc...
/// - [`blast_resistance`] value to define resistance to C4, TNT or other explosions
/// - [`opaque`] if the block is opaque or not (transparent)
/// - [`block_form`] if the block is solid or fluid
/// - [`emissive`] value to define emissive lighting
#[derive(Deserialize, Clone, Copy, Debug, Default)]
pub struct BlockStats {
    #[serde(default)]
    pub hardness: f32,
    #[serde(default)]
    pub hardness_level: u8,
    #[serde(default)]
    pub blast_resistance: f32,
    #[serde(default = "default_true")]
    pub opaque: bool,
    #[serde(default)]
    pub block_form: BlockForm,
    #[serde(default)]
    pub emissive: f32,
}

/// Block form for a block.
/// Current forms:
/// - [`Solid`] the default value for blocks, means that the player cannot go through
/// - [`Fluid`] the block is a fluid and can flow through, player can go through
#[derive(Deserialize, Clone, Copy, Debug, Default, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum BlockForm {
    #[default]
    #[serde(rename = "solid", alias = "Solid", alias = "SOLID")]
    Solid,
    #[serde(rename = "fluid", alias = "Fluid", alias = "FLUID")]
    Fluid,
}

/// Definition for a block. This is used to load the block from a file.
/// This is most used in [`BlockRegistry`] and is absolutely needed for handle the
/// block in our world.
#[derive(Clone, Debug)]
pub struct BlockDefinition {
    pub name: String,
    pub stats: BlockStats,
    pub uvs: BlockUvs,
    pub texture: Handle<Image>,
    pub material: Handle<StandardMaterial>
}

/// All blocks in the game.
/// The list displays all known blocks for the game.
/// This block can be used faster with this enum if you coded or modification the game.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Blocks {
    Border,
    Clay,
    DeepStone,
    Dirt,
    Glass,
    Grass,
    Gravel,
    Log,
    Sand,
    SandStone,
    Snow,
    Stone,
    Water
}

impl Blocks {

    /// Give the simple name back as a definition string.
    pub const fn localized_name(&self) -> &'static str {
        match self {
            Blocks::Border => "border_block",
            Blocks::Clay  => "clay_block",
            Blocks::DeepStone => "deep_stone_block",
            Blocks::Dirt  => "dirt_block",
            Blocks::Glass => "glass_block",
            Blocks::Grass => "grass_block",
            Blocks::Gravel => "gravel_block",
            Blocks::Log   => "log_block",
            Blocks::Sand  => "sand_block",
            Blocks::SandStone => "sand_stone_block",
            Blocks::Snow => "snow_block",
            Blocks::Stone => "stone_block",
            Blocks::Water => "water_block"
        }
    }

}

impl AsRef<str> for Blocks {
    fn as_ref(&self) -> &str {
        self.localized_name()
    }
}

