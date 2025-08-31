pub mod water_shader;

use crate::shader::water_shader::WaterGfxPlugin;
use bevy::prelude::*;

pub struct WorldShaderPlugin;
impl Plugin for WorldShaderPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(WaterGfxPlugin);
    }
}