use bevy::prelude::*;

#[derive(Event, Clone, Debug)]
pub struct BlockRegistryEvent {
    pub state: RegistryState,
    pub count: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RegistryState {
    Success,
    Error,
}