use std::{
    fs::File,
    io::{BufReader, Write},
    path::PathBuf,
};

#[cfg(feature = "display")]
use bevy::prelude::*;

use game::{Game, Team};
use map::Map;

pub mod ant;
pub mod game;
pub mod instruction;
pub mod map;
pub mod tile;

/// Filters a list of paths to keep only `.brain` and `.bbrain` files.
/// Directories are searched recursively.
pub fn filter_brains(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut programs = Vec::with_capacity(paths.len());
    for path in paths {
        // Traverse recursively to find all files in a directory
        if path.is_dir() {
            let paths = std::fs::read_dir(path)
                .unwrap()
                .map_while(Result::ok)
                .map(|e| e.path())
                .collect::<Vec<_>>();
            let mut programs_in_dir = filter_brains(&paths);
            programs.append(&mut programs_in_dir)
        } else if path
            .extension()
            .map_or(false, |ext| ext == "brain" || ext == "bbrain")
        {
            programs.push(path.clone())
        }
    }
    programs
}

pub fn create_game(map_path: &PathBuf, brains: &[PathBuf], seed: Option<u64>) -> Game {
    let teams = brains
        .iter()
        .enumerate()
        .filter_map(|(i, p)| {
            match Team::new_from_file(p.clone(), ('a'..'h').nth(i).unwrap(), i as u8) {
                Ok(p) => Some(p),
                Err(e) => {
                    eprintln!("Failed to parse file {p:?}:\n{e}");
                    None
                }
            }
        })
        .collect::<Vec<_>>();

    let map = Map::from_source(BufReader::new(File::open(map_path).unwrap()), &teams);

    Game::new(teams, map, seed)
}

#[cfg(not(feature = "display"))]
pub fn display_game(mut game: Game, fps: u32, tps: u32, turns: Option<usize>) {
    use std::time::Instant;

    use crossterm::{terminal, ExecutableCommand};

    let out = std::io::stdout();
    let mut frame_start = Instant::now();
    let mut tick_start = Instant::now();
    let mut ticks = 0;
    while ticks < turns.unwrap_or(Game::DEFAULT_TURNS) {
        if tick_start.elapsed().as_secs_f32() >= 1f32 / tps as f32 {
            tick_start = Instant::now();
            game.update();
            ticks += 1;
        }
        if frame_start.elapsed().as_secs_f32() >= 1f32 / fps as f32 {
            frame_start = Instant::now();

            let mut out = out.lock();
            out.execute(terminal::Clear(terminal::ClearType::All))
                .unwrap();
            writeln!(out, "turn {ticks}\n").unwrap();

            writeln!(out, "scoreboard:").unwrap();
            let mut result = Vec::from(game.teams());
            result.sort_by_key(|t| *t.score.lock().unwrap());
            result.reverse();

            writeln!(out, " {: <20}   {: <10} ", "team", "score").unwrap();
            writeln!(out, "-{:-<20}---{:-<10}-", "", "").unwrap();
            for team in result {
                writeln!(
                    out,
                    " {: <20}   {: <20} ",
                    team.name,
                    team.score.lock().unwrap()
                )
                .unwrap();
            }

            writeln!(out, "\n{game}").unwrap();
            out.flush().unwrap();
        }
    }
}

#[cfg(feature = "display")]
pub fn display_game(mut game: Game, turns: Option<usize>) {
    use bevy::sprite::MaterialMesh2dBundle;

    fn setup(
        mut commands: Commands,
        mut meshes: ResMut<Assets<Mesh>>,
        game: Res<Game>,
        mut materials: ResMut<Assets<ColorMaterial>>,
    ) {
        commands.spawn(Camera2dBundle::default());

        for row in 0..game.map().height {
            for column in 0..game.map().width {
                commands.spawn(MaterialMesh2dBundle {
                    mesh: meshes
                        .add(shape::RegularPolygon::new(50f32, 6).into())
                        .into(),
                    material: materials.add(ColorMaterial::from(Color::TURQUOISE)),
                    transform: Transform::from_translation(Vec3::new(150f32, 0f32, 0f32)),
                    ..default()
                });
            }
        }
    }

    let update = move |time: Res<Time>, mut game: ResMut<Game>| {
        println!("{}", time.delta_seconds_f64());
        game.update();
        println!("{}", game.as_ref());
    };
    App::new()
        .insert_resource(game)
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, update)
        .run()
}
