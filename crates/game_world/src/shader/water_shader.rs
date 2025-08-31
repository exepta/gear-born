use bevy::prelude::*;
use game_core::shader::water_material::{WaterMatHandle, WaterMaterial, WaterParams};
use game_core::states::{AppState, LoadingStates};
use game_core::world::block::{BlockRegistry, *};

pub struct WaterGfxPlugin;
impl Plugin for WaterGfxPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(MaterialPlugin::<WaterMaterial>::default())
            .add_systems(Update, tick_water_time);

        app.add_systems(OnEnter(AppState::Loading(LoadingStates::BaseGen)), setup_water_mat);
    }
}

pub fn setup_water_mat(
    reg: Res<BlockRegistry>,
    mut water_mats: ResMut<Assets<WaterMaterial>>,
    mut cmds: Commands,
) {
    let water_id = id_any(&reg, &["water_block", "water"]);
    let handle = make_water_material_for_atlas_tile(&reg, water_id, &mut water_mats);
    cmds.insert_resource(WaterMatHandle(handle));
}

pub fn tick_water_time(
    time: Res<Time>,
    mut mats: ResMut<Assets<WaterMaterial>>,
) {
    let dt = time.delta_secs();
    for (_, m) in mats.iter_mut() {
        m.params.t_misc.x += dt;
    }
}

fn make_water_material_for_atlas_tile(
    reg: &BlockRegistry,
    water_id: u16,
    water_mats: &mut Assets<WaterMaterial>,
) -> Handle<WaterMaterial> {
    let rect = reg.uv(water_id, Face::Top);

    let speed_u = 0.06;
    let speed_v = 0.03;
    let wave_amp = 0.05 * VOXEL_SIZE;
    let wave_freq = 0.8;

    let params = WaterParams {
        uv_rect: Vec4::new(rect.u0, rect.v0, rect.u1, rect.v1),
        flow:    Vec4::new(0.06, 0.03, 0.05 * VOXEL_SIZE, 0.8),
        t_misc:  Vec4::new(0.0, 0.15, 64.0, 0.0),
        tint:    Vec4::new(0.90, 0.95, 1.05, 0.65),
    };

    water_mats.add(WaterMaterial {
        atlas: reg.def(water_id).image.clone(),
        params,
    })
}
