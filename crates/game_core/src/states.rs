#![coverage(off)]

use bevy::prelude::*;

/// High-level application state used for managing the flow of the game.
///
/// This state machine controls the major phases such as initialization, loading, UI screens,
/// network communication, asset management, and the main in-game state.
///
/// The variants often contain their own substates for more granular control.
#[derive(States, Default, Debug, Clone, Eq, PartialEq, Hash)]
pub enum AppState {
    /// Initial startup state. Used to perform core initialization steps.
    #[default]
    AppInit,

    /// Preloading phase before the main app logic starts (e.g., loading config, pre-setup).
    Preload,

    /// Active during UI-driven screens before entering the game.
    /// Contains its own `UiState`.
    Screen(UiState),

    /// Represents asset loading logic (e.g., loading models, textures, data).
    /// Contains its own `AssetLoadState`.
    Loading(LoadingStates),

    /// State after loading and fetching is done, but before entering gameplay.
    PostLoad,

    /// Main in-game state (player can interact and play).
    /// Contains its own `InGameStates`.
    InGame(InGameStates)
}

/// UI and pre-game screen states.
///
/// Used as a substate of `AppState::Screen`.
#[derive(States, Default, Debug, Clone, Eq, PartialEq, Hash)]
pub enum UiState {
    /// Splash screen (logo, studio, etc.).
    #[default]
    Menu,
    /// Title/menu screen.
    Settings,
}


#[derive(States, Default, Debug, Clone, Eq, PartialEq, Hash)]
pub enum LoadingStates {

    #[default]
    BaseGen,

    WaterGen,

    CaveGen
}

/// Main in-game state and relevant substates.
///
/// Used as a substate of `AppState::InGame`.
#[derive(States, Default, Debug, Clone, Eq, PartialEq, Hash)]
pub enum InGameStates {
    /// Standard gameplay state.
    #[default]
    Game,
    /// The game is paused.
    Pause,
    /// Turn-based or real-time combat phase.
    Combat,
    /// Game over screen or logic.
    GameOver,
}

/// Returns `true` if the application is currently in any in‐game substate.
///
/// This run‐condition reads the global `State<AppState>` resource and checks
/// whether it is the `InGame` variant, regardless of which specific
/// in‐game substate it contains.
///
/// # Parameters
///
/// - `state`: A Res‐injected Bevy resource of type `State<AppState>`.
///
/// # Returns
///
/// `true` if the current app state matches `AppState::InGame(_)`, otherwise `false`.
pub fn is_state_in_game(state: Res<State<AppState>>) -> bool {
    matches!(*state.get(), AppState::InGame(_))
}

/// Returns `true` if the application is currently displaying a UI screen.
///
/// This run‐condition reads the global `State<AppState>` resource and checks
/// whether it is the `Screen` variant, regardless of which screen is active.
///
/// # Parameters
///
/// - `state`: A Res‐injected Bevy resource of type `State<AppState>`.
///
/// # Returns
///
/// `true` if the current app state matches `AppState::Screen(_)`, otherwise `false`.
pub fn is_state_in_ui(state: Res<State<AppState>>) -> bool {
    matches!(*state.get(), AppState::Screen(_))
}