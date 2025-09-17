use bevy::prelude::*;
use game_core::states::{AppState, BeforeUiState, LoadingStates};
use game_core::UI_ACCENT_COLOR;
/* ===========================================================
   Main Menu (Bevy 0.16.1, no bundles)
   - Centered vertical stack with three buttons:
     "Single Player", "Settings", "Exit"
   - Button background uses UI_ACCENT_COLOR, text is white.
   - Spawns a 2D camera if none exists (UI needs a camera in world).
   - "Exit" sends AppExit.
   =========================================================== */

pub struct MenuUiPlugin;

impl Plugin for MenuUiPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(AppState::Screen(BeforeUiState::Menu)), (spawn_menu_camera_if_missing, spawn_menu_ui))
            .add_systems(Update, (
                button_interactions,
                exit_on_click,
                single_player_on_click
            ).run_if(in_state(AppState::Screen(BeforeUiState::Menu))));
    }
}

/* ---------- Components ---------- */

#[derive(Component)]
struct MainMenuRoot;

#[derive(Component, Copy, Clone, Eq, PartialEq, Debug, Hash)]
enum MenuButton {
    SinglePlayer,
    Settings,
    Exit,
}

/* ---------- Systems ---------- */

/// Ensure there is at least one 2D camera so UI can render.
fn spawn_menu_camera_if_missing(mut commands: Commands, existing: Query<(), With<Camera2d>>) {
    if existing.is_empty() {
        commands.spawn((Camera2d::default(), MainMenuRoot));
    }
}

/// Build the full-screen root and a centered column with three buttons.
fn spawn_menu_ui(mut commands: Commands) {
    // Full-screen root node, centers its children.
    let root = commands
        .spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                display: Display::Flex,
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                ..Default::default()
            },
            MainMenuRoot,
            // Optional: set a transparent background so clicks don't leak through.
            BackgroundColor(Color::NONE),
        ))
        .id();

    // Column container for the buttons.
    let column = commands
        .spawn((
            Node {
                display: Display::Flex,
                flex_direction: FlexDirection::Column,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                row_gap: Val::Px(16.0),
                ..Default::default()
            },
        ))
        .id();

    commands.entity(root).add_child(column);

    // Three buttons.
    spawn_menu_button(&mut commands, column, "Single Player", MenuButton::SinglePlayer);
    spawn_menu_button(&mut commands, column, "Settings", MenuButton::Settings);
    spawn_menu_button(&mut commands, column, "Exit", MenuButton::Exit);
}

/// Helper to spawn a styled button with a text child.
fn spawn_menu_button(commands: &mut Commands, parent: Entity, label: &str, kind: MenuButton) {
    // Button entity: Node for layout + Button marker + BackgroundColor for fill.
    let button = commands
        .spawn((
            Button, // Marker; adds Interaction/FocusPolicy/Node requirements
            Node {
                width: Val::Px(300.0),
                height: Val::Px(60.0),
                display: Display::Flex,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            BackgroundColor(UI_ACCENT_COLOR), // uses your global const
            kind,
        ))
        .id();

    // Text child: white label, centered by parent Node alignment.
    let text = commands
        .spawn((
            Text::new(label),
            TextFont {
                font_size: 28.0,
                ..Default::default()
            },
            TextColor(Color::WHITE),
        ))
        .id();

    commands.entity(button).add_child(text);
    commands.entity(parent).add_child(button);
}

/// Simple hover/press feedback: tweak alpha, keep the accent hue.
fn button_interactions(
    mut q: Query<(&Interaction, &mut BackgroundColor), (Changed<Interaction>, With<Button>)>,
) {
    for (interaction, mut bg) in &mut q {
        match *interaction {
            Interaction::Hovered => {
                bg.0 = UI_ACCENT_COLOR.with_alpha(0.9);
            }
            Interaction::Pressed => {
                bg.0 = UI_ACCENT_COLOR.with_alpha(0.8);
            }
            Interaction::None => {
                bg.0 = UI_ACCENT_COLOR;
            }
        }
    }
}

/// Quit on "Exit" click. The others are stubs and can trigger your state changes later.
fn exit_on_click(
    mut q: Query<(&Interaction, &MenuButton), (Changed<Interaction>, With<Button>)>,
    mut exit_writer: EventWriter<AppExit>,
) {
    for (interaction, which) in &mut q {
        if *interaction == Interaction::Pressed && *which == MenuButton::Exit {
            exit_writer.write(AppExit::Success);
        }
    }
}

fn single_player_on_click(
    mut q: Query<(&Interaction, &MenuButton), (Changed<Interaction>, With<Button>)>,
    mut next_state: ResMut<NextState<AppState>>,
    mut commands: Commands,
    roots: Query<Entity, With<MainMenuRoot>>,
) {
    for (interaction, which) in &mut q {
        if *interaction == Interaction::Pressed && *which == MenuButton::SinglePlayer {
            next_state.set(AppState::Loading(LoadingStates::BaseGen));

            for root in &roots {
                commands.entity(root).despawn();
            }
        }
    }
}