#![coverage(off)]

use bevy::prelude::*;

/// Represents the state of the World Inspector UI.
///
/// This resource holds a single boolean value indicating whether the World Inspector UI
/// is currently visible or hidden. The state can be toggled by user input (e.g., a key press),
/// and this struct is used to track the visibility of the World Inspector in the application.
///
/// The `WorldInspectorState` is initialized to `false` (hidden) by default.
///
/// # Fields
///
/// * `0`: A boolean value that represents the visibility of the World Inspector UI.
///   - `true`: The World Inspector is visible.
///   - `false`: The World Inspector is hidden.
#[derive(Resource, Default, Debug)]
pub struct WorldInspectorState(pub bool);