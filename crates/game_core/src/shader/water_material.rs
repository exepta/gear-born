use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef, ShaderType};

#[derive(Clone, Copy, Default, ShaderType, Debug)]
pub struct WaterParams {
    pub uv_rect: Vec4,
    pub flow:    Vec4, // speed_u, speed_v, amp, freq
    pub t_misc:  Vec4, // time, _, _, _
    pub tint:    Vec4,
}

#[derive(AsBindGroup, Asset, TypePath, Clone, Debug)]
pub struct WaterMaterial {
    #[uniform(0, visibility = "VertexFragment")]
    pub params: WaterParams,

    #[texture(1)]
    #[sampler(2)]
    pub atlas: Handle<Image>,
}

#[derive(Resource, Clone)]
pub struct WaterMatHandle(pub Handle<WaterMaterial>);

impl Material for WaterMaterial {
    fn vertex_shader() -> ShaderRef { ShaderRef::Path("shaders/water.wgsl".into()) }
    fn fragment_shader() -> ShaderRef { ShaderRef::Path("shaders/water.wgsl".into()) }
    fn alpha_mode(&self) -> AlphaMode { AlphaMode::Blend }
}