// shaders/water.wgsl

#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::mesh_bindings
#import bevy_pbr::mesh_functions
#import bevy_pbr::view_transformations::position_world_to_clip

struct VOut {
  @builtin(position) clip: vec4<f32>,
  @location(0) uv: vec2<f32>,
  @location(1) world_pos: vec3<f32>,
  @location(2) normal_ws: vec3<f32>,
};

struct VertexInput {
  @location(0) position: vec3<f32>,
  @location(1) normal:   vec3<f32>,
  @location(2) uv:       vec2<f32>,
  @builtin(instance_index) instance_index: u32,
};

// ► MATERIAL ist group(2), NICHT group(1)!
struct WaterParams {
  uv_rect: vec4<f32>,
  flow:    vec4<f32>,
  t_misc:  vec4<f32>,
  tint:    vec4<f32>,
};
@group(2) @binding(0) var<uniform> params: WaterParams;
@group(2) @binding(1) var atlas_tex: texture_2d<f32>;
@group(2) @binding(2) var atlas_smp: sampler;

fn remap_uv_scroll(uv_in: vec2<f32>, normal_ws: vec3<f32>) -> vec2<f32> {
  let u0 = params.uv_rect.x;
  let v0 = params.uv_rect.y;
  let u1 = params.uv_rect.z;
  let v1 = params.uv_rect.w;
  let du = u1 - u0;
  let dv = v1 - v0;

  let uv_local = (uv_in - vec2<f32>(u0, v0)) / vec2<f32>(du, dv);
  let tilt = clamp(normal_ws.y, 0.0, 1.0);
  let speed_scale = mix(0.35, 1.0, tilt);
  let speed = params.flow.xy * speed_scale;
  let t = params.t_misc.x;
  let scrolled = fract(uv_local + speed * t);
  return vec2<f32>(u0, v0) + scrolled * vec2<f32>(du, dv);
}

@vertex
fn vertex(v: VertexInput) -> VOut {
  var out: VOut;

  let world_from_local = bevy_pbr::mesh_functions::get_world_from_local(v.instance_index);
  let world_pos4       = world_from_local * vec4<f32>(v.position, 1.0);
  let n_ws             = bevy_pbr::mesh_functions::mesh_normal_local_to_world(v.normal, v.instance_index);

  var pos_world = world_pos4.xyz;

  if (n_ws.y > 0.99) {
    let amp  = params.flow.z;
    let freq = params.flow.w;
    let t    = params.t_misc.x;
    let w = sin(pos_world.x * freq + t * 1.3)
          + 0.6 * sin(pos_world.z * (freq * 1.12) + t * 0.9);
    pos_world.y = pos_world.y + w * amp;
  }

  out.world_pos = pos_world;
  out.normal_ws = normalize(n_ws);
  out.clip      = position_world_to_clip(pos_world);
  out.uv        = v.uv;
  return out;
}

@fragment
fn fragment(in: VOut) -> @location(0) vec4<f32> {
  let view = bevy_pbr::mesh_view_bindings::view;

  let uv  = remap_uv_scroll(in.uv, in.normal_ws);
  let col = textureSample(atlas_tex, atlas_smp, uv);

  let cam_pos = (view.world_from_view * vec4<f32>(0.0, 0.0, 0.0, 1.0)).xyz;
  let V = normalize(cam_pos - in.world_pos);
  let N = normalize(in.normal_ws);
  let fres = pow(1.0 - clamp(dot(N, V), 0.0, 1.0), 3.0);

  let rgb_fresnel = mix(col.rgb * 0.90, col.rgb * 1.10, fres);
  let rgb_tinted  = rgb_fresnel * params.tint.rgb;
  let a_tinted    = col.a * params.tint.a;
  return vec4<f32>(rgb_tinted, a_tinted);
}
