use bevy::prelude::*;
use bevy::render::view::RenderLayers;
use game_core::debug::SelectionGizmoGroup;
use game_core::player::selection::SelectionState;
use game_core::states::{AppState, InGameStates};
use game_core::world::block::{get_block_world, BlockRegistry, SelectedBlock, VOXEL_SIZE};
use game_core::world::chunk::{ChunkMap, VoxelStage};
use game_core::world::ray_cast_voxels;
use game_core::BlockCatalogPreviewCam;

pub struct LookAtService;

impl Plugin for LookAtService {
    fn build(&self, app: &mut App) {
        app.init_resource::<GizmoConfigStore>()
            .init_gizmo_group::<SelectionGizmoGroup>()
            .add_systems(Startup, setup_selection_gizmo_config);

        app.configure_sets(Update, (VoxelStage::Input, VoxelStage::WorldEdit, VoxelStage::Meshing).chain());

        app.add_systems(
            Update,
            (
                update_selection.in_set(VoxelStage::Input),
                draw_selection_gizmo.in_set(VoxelStage::Input),
                pick_block_from_look,
            ).chain().run_if(in_state(AppState::InGame(InGameStates::Game)))
        );
    }
}

fn setup_selection_gizmo_config(mut store: ResMut<GizmoConfigStore>) {
    let (cfg, _) = store.config_mut::<SelectionGizmoGroup>();
    cfg.line = GizmoLineConfig { width: 5.0, ..default() };
    cfg.render_layers = RenderLayers::layer(2);
    cfg.depth_bias = 0.0;
}

fn update_selection(
    mut sel: ResMut<SelectionState>,
    q_cam: Query<(&GlobalTransform, &Camera), (With<Camera3d>, Without<BlockCatalogPreviewCam>)>,
    chunk_map: Res<ChunkMap>,
) {
    let Ok((tf, _cam)) = q_cam.single() else { return; };

    let origin_bs = tf.translation() / VOXEL_SIZE;
    let dir_bs: Vec3 = tf.forward().into();
    let max_dist_blocks = 8.0;

    sel.hit = ray_cast_voxels(origin_bs, dir_bs, max_dist_blocks, &chunk_map);
}

fn draw_selection_gizmo(
    sel: Res<SelectionState>,
    mut gizmos: Gizmos<SelectionGizmoGroup>,
) {
    if let Some(hit) = sel.hit {
        let s = VOXEL_SIZE;
        let center = Vec3::new(
            (hit.block_pos.x as f32 + 0.5) * s,
            (hit.block_pos.y as f32 + 0.5) * s,
            (hit.block_pos.z as f32 + 0.5) * s,
        );
        let size = Vec3::splat(1.002 * s);
        gizmos.cuboid(Transform::from_translation(center).with_scale(size), Color::BLACK);
    }
}

fn pick_block_from_look(
    buttons: Res<ButtonInput<MouseButton>>,
    sel_state: Res<SelectionState>,
    chunk_map: Res<ChunkMap>,
    reg: Res<BlockRegistry>,
    mut selected: ResMut<SelectedBlock>,
) {
    if !buttons.just_pressed(MouseButton::Middle) { return; }
    let Some(hit) = sel_state.hit else { return; };

    let id = get_block_world(&chunk_map, hit.block_pos);
    if id == 0 { return; }

    selected.id = id;
    selected.name = reg.name_opt(id).unwrap_or("").to_string();
    debug!("Picked block: {} ({})", selected.name, selected.id);
}