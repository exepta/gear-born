use bevy::pbr::{MaterialPipeline, MaterialPipelineKey};
use bevy::prelude::*;
use bevy::render::mesh::MeshVertexBufferLayoutRef;
use bevy::render::render_resource::*;
pub use water_types::WaterParams;

mod water_types {
    #![allow(dead_code)]
    use bevy::prelude::*;
    use bevy::render::render_resource::ShaderType;

    #[derive(Clone, Copy, Default, ShaderType, Debug)]
    pub struct WaterParams {
        pub uv_rect: Vec4,
        pub flow:    Vec4,
        pub t_misc:  Vec4,
        pub tint:    Vec4,
    }
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
    fn alpha_mode(&self) -> AlphaMode { AlphaMode::Premultiplied }

    fn specialize(
        _pipeline: &MaterialPipeline<Self>,
        descriptor: &mut RenderPipelineDescriptor,
        _layout: &MeshVertexBufferLayoutRef,
        _key: MaterialPipelineKey<Self>,
    ) -> Result<(), SpecializedMeshPipelineError> {
        if let Some(ds) = descriptor.depth_stencil.as_mut() {
            ds.depth_write_enabled = true;
        }
        if let Some(fragment) = descriptor.fragment.as_mut() {
            if let Some(Some(tgt)) = fragment.targets.get_mut(0) {
                tgt.blend = Some(BlendState::PREMULTIPLIED_ALPHA_BLENDING);
            }
        }
        Ok(())
    }
}