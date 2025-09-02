use bevy::prelude::*;
use game_core::DefaultGameFont;

pub struct DefaultFontHandler;

impl Plugin for DefaultFontHandler {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, load_default_font);
        app.add_systems(Update, add_to_texts.run_if(resource_exists::<DefaultGameFont>));
    }
}

fn load_default_font(mut commands: Commands, asset_server: Res<AssetServer>) {
    let font_handle = asset_server.load("fonts/roboto/roboto.ttf");
    commands.insert_resource(DefaultGameFont(font_handle));
}

fn add_to_texts(default_font: Res<DefaultGameFont>, mut q: Query<&mut TextFont, Added<Text>>) {
    for mut text_font in &mut q {
        text_font.font = default_font.0.clone();
    }
}