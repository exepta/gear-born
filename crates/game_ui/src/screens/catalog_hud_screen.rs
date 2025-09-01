use bevy::prelude::*;
use bevy::render::camera::{ImageRenderTarget, RenderTarget};
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages};
use bevy::render::view::RenderLayers;
use bevy::ui::FocusPolicy;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use game_core::states::{AppState, InGameStates};
use game_core::world::block::*;
use game_core::{BlockCatalogPreviewCam, BlockCatalogUiState, UI_ACCENT_COLOR};

/* ---------------- Plugin ---------------- */

pub struct BlockCatalogUiPlugin;

impl Plugin for BlockCatalogUiPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<BlockCatalogUiState>()
            .add_systems(
                OnEnter(AppState::InGame(InGameStates::Game)),
                spawn_block_catalog_ui.run_if(resource_exists::<BlockRegistry>),
            )
            .add_systems(
                Update,
                (
                    toggle_block_catalog_ui,
                    handle_block_pick_interactions,
                    spin_previews,
                    update_selected_highlight
                        .run_if(resource_changed::<SelectedBlock>),
                )
                    .run_if(in_state(AppState::InGame(InGameStates::Game)))
                    .run_if(resource_exists::<BlockCatalogUiState>)
                    .run_if(resource_exists::<BlockRegistry>),
            );
    }
}

/* ---------------- State / Components ---------------- */

#[derive(Component)] struct BlockCatalogRoot;
#[derive(Component)] struct BlockItemButton { block_id: BlockId }
#[derive(Component)] struct PreviewAnchor; // rotates, centered at origin
#[derive(Component)] struct PreviewMesh;
#[derive(Component)] struct Spin { speed: f32 }

/* ---------------- Constants ---------------- */

const PANEL_WIDTH_PX: f32 = 580.0;
const PREVIEW_SIZE_PX: u32 = 100;
const PREVIEW_LAYER_BASE: usize = 3;
const PREVIEW_CAM_FOV_DEG: f32 = 32.0;

/* ---------------- Systems ---------------- */

fn toggle_block_catalog_ui(
    keys: Res<ButtonInput<KeyCode>>,
    mut ui: ResMut<BlockCatalogUiState>,
    mut q_vis: Query<&mut Visibility, With<BlockCatalogRoot>>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
) {
    if !keys.just_pressed(KeyCode::Tab) { return; }
    ui.open = !ui.open;
    if let Some(root) = ui.root {
        if let Ok(mut v) = q_vis.get_mut(root) {
            *v = if ui.open { Visibility::Visible } else { Visibility::Hidden };
        }
    }
    if let Ok(mut win) = windows.single_mut() {
        if ui.open {
            win.cursor_options.grab_mode = CursorGrabMode::None;
            win.cursor_options.visible = true;
        } else {
            win.cursor_options.grab_mode = CursorGrabMode::Locked;
            win.cursor_options.visible = false;
        }
    }
}

fn handle_block_pick_interactions(
    mut q_btn: Query<
        (&Interaction, &BlockItemButton, Option<&mut BackgroundColor>),
        (Changed<Interaction>, With<Button>)
    >,
    mut selected: ResMut<SelectedBlock>,
    reg: Res<BlockRegistry>,
) {
    for (interaction, item, bg_opt) in &mut q_btn {
        let is_selected = item.block_id == selected.id;

        match *interaction {
            Interaction::Pressed => {
                selected.id = item.block_id;
                selected.name = reg.name(item.block_id).to_string();
            }
            Interaction::Hovered => {
                if !is_selected {
                    if let Some(mut bg) = bg_opt {
                        *bg = BackgroundColor(Color::srgb_u8(120, 120, 120));
                    }
                }
            }
            Interaction::None => {
                if !is_selected {
                    if let Some(mut bg) = bg_opt {
                        *bg = BackgroundColor(Color::srgb_u8(80, 80, 80));
                    }
                }
            }
        }
    }
}

fn spin_previews(time: Res<Time>, mut q: Query<(&mut Transform, &Spin), With<PreviewAnchor>>) {
    for (mut tf, spin) in &mut q {
        tf.rotate_y(spin.speed * time.delta_secs());
    }
}

fn update_selected_highlight(
    selected: Res<SelectedBlock>,
    mut q_borders: Query<(&BlockItemButton, &mut BorderColor)>,
    mut q_labels:  Query<(&BlockItemButton,  &mut BackgroundColor)>,
) {
    let normal_border  = Color::srgba(1.0, 1.0, 1.0, 0.08);
    let normal_back    = Color::srgb_u8(80, 80, 80);

    for (btn, mut border) in &mut q_borders {
        *border = BorderColor(if btn.block_id == selected.id { UI_ACCENT_COLOR } else { normal_border });
    }
    for (lbl, mut back_color) in &mut q_labels {
        *back_color = BackgroundColor(if lbl.block_id == selected.id { UI_ACCENT_COLOR } else { normal_back });
    }
}

/* ---------------- Spawn UI + Previews ---------------- */

fn spawn_block_catalog_ui(
    mut commands: Commands,
    mut ui: ResMut<BlockCatalogUiState>,
    reg: Res<BlockRegistry>,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>, // <-- add
) {
    // Root panel
    let root = commands
        .spawn((
            BlockCatalogRoot,
            Node {
                position_type: PositionType::Absolute,
                right: Val::Px(0.0),
                top: Val::Px(0.0),
                width: Val::Px(PANEL_WIDTH_PX),
                height: Val::Percent(100.0),
                padding: UiRect::axes(Val::Px(20.0), Val::Px(20.0)),
                flex_direction: FlexDirection::Column,
                display: Display::Flex,
                ..default()
            },
            BackgroundColor(Color::srgba(0.06, 0.06, 0.07, 0.92)),
            Visibility::Hidden,
            Name::new("BlockCatalog:Root"),
        ))
        .id();

    // Header
    let header = commands
        .spawn((
            Node {
                height: Val::Px(40.0),
                justify_content: JustifyContent::SpaceBetween,
                align_items: AlignItems::Center,
                ..default()
            },
            Visibility::default(),
            Name::new("BlockCatalog:Header"),
        ))
        .with_children(|c| {
            c.spawn((
                Text::new("Blocks"),
                TextFont { font_size: 20.0, ..default() },
                TextColor(Color::WHITE),
                Visibility::default(),
            ));
            c.spawn((
                Text::new("(TAB to toggle)"),
                TextFont { font_size: 14.0, ..default() },
                TextColor(Color::srgba(1.0,1.0,1.0,0.6)),
                Visibility::default(),
            ));
        })
        .id();

    // Grid
    let grid = commands
        .spawn((
            Node {
                flex_grow: 1.0,
                flex_wrap: FlexWrap::Wrap,
                align_content: AlignContent::FlexStart,
                align_items: AlignItems::FlexStart,
                column_gap: Val::Px(10.0),
                row_gap: Val::Px(10.0),
                overflow: Overflow::clip_y(),
                ..default()
            },
            Visibility::default(),
            Name::new("BlockCatalog:Grid"),
        ))
        .id();

    commands.entity(root).add_child(header);
    commands.entity(root).add_child(grid);

    for (block_id_idx, def) in reg.defs.iter().enumerate() {
        let block_id = block_id_idx as BlockId;
        if block_id == 0 { continue; }

        let layer_idx = (PREVIEW_LAYER_BASE + block_id_idx) % 32;
        let layer = RenderLayers::layer(layer_idx);

        let rt = make_render_texture(&mut images, PREVIEW_SIZE_PX, PREVIEW_SIZE_PX);
        let rt_for_ui = rt.clone();

        // Per-item camera
        commands.spawn((
            BlockCatalogPreviewCam,
            Camera3d::default(),
            Camera {
                clear_color: ClearColorConfig::Custom(Color::srgba(0.20, 0.22, 0.26, 1.0)),
                target: RenderTarget::Image(ImageRenderTarget::from(rt)),
                ..default()
            },
            Projection::Perspective(PerspectiveProjection {
                fov: PREVIEW_CAM_FOV_DEG.to_radians(),
                near: 0.01,
                far: 50.0,
                ..default()
            }),
            Transform::from_xyz(2.05, 1.7, 2.05).looking_at(Vec3::ZERO, Vec3::Y),
            GlobalTransform::default(),
            Visibility::default(),
            layer.clone(), // <-- unique layer
            Name::new(format!("Preview:Cam({})", def.name)),
        ));

        // Rotating anchor
        let anchor = commands
            .spawn((
                PreviewAnchor,
                Spin { speed: 0.8 },
                Transform::from_rotation(Quat::from_euler(EulerRot::XYZ, 0.0, 0.0, 0.0)),
                GlobalTransform::default(),
                Visibility::default(),
                layer.clone(),
                Name::new(format!("Preview:Anchor({})", def.name)),
            ))
            .id();

        // **Unlit preview material** so textures always show
        let preview_mat = materials.add(StandardMaterial {
            base_color_texture: Some(def.image.clone()),
            alpha_mode: if def.stats.opaque { AlphaMode::Opaque } else { AlphaMode::Blend },
            base_color: if def.stats.opaque { Color::WHITE } else { Color::srgba(1.0,1.0,1.0,0.9) },
            unlit: true,
            perceptual_roughness: 1.0,
            metallic: 0.0,
            reflectance: 0.0,
            ..default()
        });

        // Mesh
        let mesh = build_block_cube_mesh(&reg, block_id, 1.0);
        commands.entity(anchor).with_children(|c| {
            c.spawn((
                PreviewMesh,
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(preview_mat.clone()),
                Transform::from_translation(Vec3::splat(-0.5))
                    .with_scale(Vec3::splat(0.75)),
                GlobalTransform::default(),
                Visibility::default(),
                layer.clone(),
                Name::new(format!("Preview:Mesh({})", def.name)),
            ));
        });

        // UI Button
        let button = commands
            .spawn((
                Button,
                BlockItemButton { block_id },
                Node {
                    width: Val::Px(PREVIEW_SIZE_PX as f32),
                    height: Val::Px(PREVIEW_SIZE_PX as f32 + 28.0),
                    flex_direction: FlexDirection::Column,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    padding: UiRect::all(Val::Px(6.0)),
                    border: UiRect::all(Val::Px(2.0)),
                    ..default()
                },
                BorderColor(Color::srgba(1.0, 1.0, 1.0, 0.08)),
                BackgroundColor(Color::srgb_u8(80, 80, 80)),
                Visibility::default(),
                Name::new(format!("BlockItem:{}", def.name)),
            ))
            .with_children(|c| {
                c.spawn((
                    Node {
                        width: Val::Px(PREVIEW_SIZE_PX as f32),
                        height: Val::Px(PREVIEW_SIZE_PX as f32),
                        ..default()
                    },
                    ImageNode::new(rt_for_ui),
                    FocusPolicy::Pass,
                    Visibility::default(),
                    Name::new("BlockItem:Preview"),
                ));
                c.spawn((
                    Text::new(def.name.clone()),
                    TextFont { font_size: 13.0, ..default() },
                    TextColor(Color::srgba(1.0, 1.0, 1.0, 0.85)),
                    FocusPolicy::Pass,
                    Visibility::default(),
                    Name::new("BlockItem:Label"),
                ));
            })
            .id();

        commands.entity(grid).add_child(button);
    }

    ui.root = Some(root);
}

/* ---------------- Helpers ---------------- */

fn make_render_texture(images: &mut Assets<Image>, w: u32, h: u32) -> Handle<Image> {
    let mut image = Image::new_fill(
        Extent3d { width: w, height: h, depth_or_array_layers: 1 },
        TextureDimension::D2,
        &[0, 0, 0, 0],
        TextureFormat::bevy_default(),
        RenderAssetUsages::RENDER_WORLD | RenderAssetUsages::MAIN_WORLD,
    );
    image.texture_descriptor.usage =
        TextureUsages::RENDER_ATTACHMENT
            | TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_SRC
            | TextureUsages::COPY_DST;
    images.add(image)
}
