use clap::Parser;
use itertools::Itertools;
use mechants::instruction::Program;
use mechants::{create_game, display_game, filter_brains};
use std::path::{Path, PathBuf};

#[derive(clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Arguments {
    #[arg(short, long, help = "Prints more information")]
    pub verbose: bool,
    #[arg(short, long, help = "Uses a fixed seed for matches")]
    pub seed: Option<u64>,
    #[arg(short, long, help = "Changes the number of turns played in a match")]
    pub turns: Option<usize>,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand, Clone)]
pub enum Command {
    #[command(about = "Plays a match between multiple opponents on a given map")]
    Match {
        #[arg(help = "Path to the map on which the match will be played")]
        map: PathBuf,
        #[arg(help = "Paths to the programs for each team")]
        brains: Vec<PathBuf>,
        #[arg(
            short,
            long,
            help = "Enables the graphical interface to display the match"
        )]
        display: bool,
    },
    #[command(about = "Round-Robin tournament")]
    RoundRobin {
        #[arg(help = "Path to the map on which the tournament will be played")]
        map: PathBuf,
        #[arg(help = "Paths to the programs for each team")]
        brains: Vec<PathBuf>,
    },
    #[command(about = "Double elimination tournament, with an optional seeding")]
    Tournament {
        #[arg(help = "Path to the map on which the tournament will be played")]
        map: PathBuf,
        #[arg(help = "Paths to the programs for each team")]
        brains: Vec<PathBuf>,
        #[arg(
            short,
            long,
            help = "Seeding to apply (the file will be read from best seed to worse seed)"
        )]
        seeding: Option<PathBuf>,
    },

    #[command(about = "Checks a .brain file for syntax errors")]
    Check { program: PathBuf },
    #[command(about = "Compiles a .brain file to binary format, reducing size by a factor of ~2")]
    ToBin {
        input: PathBuf,
        #[arg(help = "Output file, if none is provided defaults to \"./<input>.bbrain\"")]
        output: Option<PathBuf>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Arguments::parse();

    match args.command {
        Command::Check { program } => {
            Program::from_file(program)?;
            Ok(())
        }
        Command::ToBin { input, output } => {
            let output = output.unwrap_or({
                let file_name = input.file_stem().unwrap().to_str().unwrap();
                Path::new(file_name).with_extension("bbrain")
            });
            Program::from_file(input)?.to_binary(output)?;
            Ok(())
        }
        Command::Match {
            map,
            brains,
            display,
        } => {
            let brains = filter_brains(&brains);
            let mut game = create_game(&map, &brains, args.seed);

            if display {
                display_game(game, 60, 128, args.turns);
            } else {
                game.play(args.turns);
                let mut result = Vec::from(game.teams());
                result.sort_by_key(|t| *t.score.lock().unwrap());
                result.reverse();

                println!(" {: <20} | {: <20} ", "team", "score");
                for team in result {
                    println!(" {: <20} | {: <20} ", team.name, team.score.lock().unwrap());
                }
            }

            Ok(())
        }
        Command::RoundRobin { map, brains } => {
            let brains = filter_brains(&brains);
            let mut tournament_scores = vec![0; brains.len()];

            // For all brains, play against all others
            for contestants in brains.iter().enumerate().combinations(2) {
                let brains = contestants
                    .iter()
                    .map(|(_, b)| (*b).clone())
                    .collect::<Vec<_>>();
                let mut game = create_game(&map, &brains, args.seed);
                game.play(args.turns);

                let result = game.teams();
                let winner = result
                    .iter()
                    .enumerate()
                    .max_by_key(|(_, t)| *t.score.lock().unwrap())
                    .map(|(i, _)| i)
                    .unwrap();

                tournament_scores[contestants[winner].0] += 1;
            }

            // Then display the final scoreboard
            let mut scoreboard = tournament_scores
                .iter()
                .copied()
                .zip(
                    brains
                        .iter()
                        .map(|p| p.file_stem().unwrap().to_str().unwrap()),
                )
                .collect::<Vec<_>>();
            scoreboard.sort_by_key(|(s, _)| *s);
            println!(" {: <20} | {: <20}", "team", "score");
            for (score, team) in scoreboard {
                println!(" {team: <20} | {score: <20} ")
            }

            Ok(())
        }
        Command::Tournament { .. } => {
            println!("eheh this is not yet implemented : oupsh :))))");
            /*
            let brains = filter_brains(&brains);
            let mut winner_bracket = (0..brains.len()).collect::<Vec<_>>();
            let mut looser_bracket: Vec<usize> = vec![];
            let mut eliminated: Vec<usize> = vec![];

            // Deals with seeding
            if let Some(seeding) = seeding {
                let file = BufReader::new(File::open(seeding)?);
                let seeding = file.lines().map_while(Result::ok).collect::<Vec<_>>();
                winner_bracket.sort_by(|i, j| {
                    let i_team_name = brains[*i].file_stem().unwrap().to_str().unwrap();
                    let j_team_name = brains[*j].file_stem().unwrap().to_str().unwrap();
                    let i_seed = seeding
                        .iter()
                        .position(|t| t == i_team_name)
                        .unwrap_or(usize::MAX);
                    let j_seed = seeding
                        .iter()
                        .position(|t| t == j_team_name)
                        .unwrap_or(usize::MAX);
                    j_seed.cmp(&i_seed)
                })
            }

            // Players are matched two-by-two in both brackets until only two are left
            // To account for byes (case where the number of teams isn't a power of two),
            // the brackets are traversed in reverse on uneven turns.
            while winner_bracket.len() > 1 || !looser_bracket.is_empty() {
                let mut new_loosers = vec![];
                for (i, j) in winner_bracket.iter().tuple_windows() {
                    let result = play_match(&map, &[a.clone(), b.clone()], args.turns, args.seed);
                }
            }
            */

            Ok(())
        }
    }
}
