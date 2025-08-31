// shaders/water.wgsl

#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::prepass_utils::prepass_depth
#import bevy_pbr::view_transformations::{
  position_world_to_clip,
  ndc_to_frag_coord,
  depth_ndc_to_view_z,
}

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

// ► MATERIAL ist group(2)
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

  let amp  = params.flow.z;
  let freq = params.flow.w;
  let t    = params.t_misc.x;
  let wave = ( sin(pos_world.x * freq + t * 1.3)
             + 0.6 * sin(pos_world.z * (freq * 1.12) + t * 0.9) ) * amp;

  // Boden bleibt ruhig
  let is_bottom = n_ws.y < -0.95;
  if (!is_bottom) {
    pos_world.y = pos_world.y + wave;
  }

  out.world_pos = pos_world;
  out.normal_ws = normalize(n_ws);
  out.clip      = position_world_to_clip(pos_world);
  out.uv        = v.uv;
  return out;
}

@fragment
fn fragment(in: VOut) -> @location(0) vec4<f32> {
  let view    = bevy_pbr::mesh_view_bindings::view;
  let cam_pos = (view.world_from_view * vec4<f32>(0.0,0.0,0.0,1.0)).xyz;
  let V       = normalize(cam_pos - in.world_pos);
  let N       = normalize(in.normal_ws);

  // --- Seiten von innen unsichtbar machen ---
  let is_side     = abs(N.y) < 0.4;
  let from_inside = dot(N, V) < 0.0;
  let camera_below_side = cam_pos.y < in.world_pos.y;

  if (is_side && (from_inside || camera_below_side)) {
    discard;
  }

  // --- UV / Textur ---
  let uv  = remap_uv_scroll(in.uv, in.normal_ws);
  let tex = textureSample(atlas_tex, atlas_smp, uv);

  // --- Fresnel ---
  let fres = pow(1.0 - clamp(dot(N, V), 0.0, 1.0), 3.0);

  // --- Depth Fade (Bodenkontakt) ---
  var contact: f32 = 0.0;
  #ifdef DEPTH_PREPASS
    let clip    = position_world_to_clip(in.world_pos);
    let ndc     = clip.xyz / clip.w;
    let frag_xy = ndc_to_frag_coord(ndc.xy);

    let scene_ndc   = prepass_depth(vec4<f32>(frag_xy, 0.0, 0.0), 0u);
    let scene_viewz = depth_ndc_to_view_z(scene_ndc);
    let my_viewz    = depth_ndc_to_view_z(ndc.z);
    let dist_view   = abs(scene_viewz - my_viewz);

    let fade    = max(params.t_misc.w, 0.01);        // Meter
    contact     = 1.0 - smoothstep(0.0, fade, dist_view);
  #endif

  // --- Lighting ---
  let base_rgb = tex.rgb * params.tint.rgb;
  let fres_rgb = mix(base_rgb * 0.90, base_rgb * 1.10, fres);

  let L      = normalize(vec3<f32>(0.35, 1.0, 0.2));
  let H      = normalize(L + V);
  let spec_p = max(params.t_misc.z, 1.0);
  let spec_i = params.t_misc.y;
  let spec   = pow(max(dot(N, H), 0.0), spec_p) * spec_i;

  // Kontakt dunkler & minimal dichter
  let contact_dark = mix(1.0, 0.82, contact);
  let a_base       = params.tint.a;
  let a_final      = clamp(a_base + contact * 0.12, 0.0, 1.0);

  let rgb_lit    = (fres_rgb * contact_dark) + vec3<f32>(spec);
  let rgb_premul = rgb_lit * a_final; // premultiplied
  return vec4<f32>(rgb_premul, a_final);
}
