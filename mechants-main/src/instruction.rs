//! Instructions are atomic operations used to program ants.

use std::collections::HashMap;
use std::fmt::Formatter;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader};
use std::path::Path;
use thiserror::Error;

pub type Address = u32;
pub type PheromoneIndex = u8;

#[derive(Error, Debug)]
pub enum InstructionParseError {
    #[error("Undefined label: {0}")]
    UndefinedLabel(String),
    #[error("Invalid instruction: {0}")]
    InvalidInstruction(String),
    #[error("Tried parsing an empty string")]
    EmptyInstruction,
    #[error("Missing an argument")]
    MissingArgument,
    #[error("Invalid value for an argument")]
    InvalidArgument,
}
#[derive(Error, Debug)]
pub enum ProgramParseError {
    #[error("Error when opening file: {0:?}")]
    FileError(Box<dyn std::error::Error>),
    #[error("Program contained errors:\n{0:?}")]
    InstructionParseErrors(InstructionParseErrors),
    #[error("Label {0} appears multiple times")]
    NonUniqueLabel(String),
    #[error("A program must begin with a label, not an instruction")]
    StartsWithInstruction,
    #[error("Empty blocks are not permitted:\n{block_label}: <- line {line}\n\t[at least one instruction needed]")]
    EmptyBlock { block_label: String, line: u32 },
    #[error("Input program file has an unknown extension")]
    UnknownExtension,
    #[error("Input binary file failed to be decoded: {0:?}")]
    BinaryDecodeError(Box<dyn std::error::Error>),
}
pub struct InstructionParseErrors(Vec<(String, u32, String, InstructionParseError)>);
impl std::fmt::Debug for InstructionParseErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (block_label, line, instruction, error) in &self.0 {
            writeln!(
                f,
                "{error}\n{block_label}:\n\t[start of block]\n\t{instruction} <- line {line}\n\t[rest of block]"
            )?;
            writeln!(f)?
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, bincode::Encode, bincode::Decode)]
pub struct Program(pub Vec<Instruction>);
impl Program {
    pub fn from_file<P: AsRef<Path>>(file: P) -> Result<Self, ProgramParseError> {
        match file.as_ref().extension().and_then(|e| e.to_str()) {
            Some("bbrain") => Self::from_binary(file),
            Some("brain") => Self::from_ascii(file),
            _ => Err(ProgramParseError::UnknownExtension),
        }
    }
    pub fn from_ascii<P: AsRef<Path>>(file: P) -> Result<Self, ProgramParseError> {
        struct Block {
            label: String,
            line: Address,
            offset: Address,
            instructions: Vec<String>,
        }

        let reader = BufReader::new(
            File::open(file).map_err(|e| ProgramParseError::FileError(Box::new(e)))?,
        );

        // First find labels and map them to the corresponding instruction.
        let mut blocks = Vec::new();
        for (i, line) in reader.lines().map_while(Result::ok).enumerate() {
            let Some(line) = line.split(';').next() else {
                continue;
            };
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            // Labels must end with a `:`
            if line.contains(':') {
                blocks.push(Block {
                    label: line[..line.len() - 1].to_string(),
                    line: i as u32,
                    offset: blocks
                        .last()
                        .map(|b: &Block| b.offset + b.instructions.len() as Address)
                        .unwrap_or(0),
                    instructions: Vec::new(),
                });
            } else {
                let block = blocks
                    .last_mut()
                    .ok_or(ProgramParseError::StartsWithInstruction)?;
                block.instructions.push(line.to_string());
            }
        }

        // We then parse instructions one by one, making sure that blocks end with
        // a jump instruction.
        let mut instructions = Vec::new();
        let mut parse_errors = Vec::new();
        let mut labels = HashMap::new();
        for block in &blocks {
            if labels.insert(block.label.as_str(), block.offset).is_some() {
                return Err(ProgramParseError::NonUniqueLabel(block.label.clone()));
            }
        }

        for block in &blocks {
            if block.instructions.is_empty() {
                return Err(ProgramParseError::EmptyBlock {
                    block_label: block.label.clone(),
                    line: block.line,
                });
            }

            for (i, instruction) in block.instructions.iter().enumerate() {
                match Instruction::try_from((instruction.as_str(), &labels)) {
                    Ok(instr) => instructions.push(instr),
                    Err(e) => parse_errors.push((
                        block.label.to_string(),
                        block.line + i as Address,
                        instruction.to_string(),
                        e,
                    )),
                }
            }
        }

        if parse_errors.is_empty() {
            Ok(Self(instructions))
        } else {
            Err(ProgramParseError::InstructionParseErrors(
                InstructionParseErrors(parse_errors),
            ))
        }
    }

    pub fn to_binary<P: AsRef<Path>>(&self, output: P) -> Result<(), Box<dyn std::error::Error>> {
        let mut file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&output)?;
        bincode::encode_into_std_write(
            self,
            &mut file,
            bincode::config::standard().with_fixed_int_encoding(),
        )?;
        Ok(())
    }
    pub fn from_binary<P: AsRef<Path>>(file: P) -> Result<Self, ProgramParseError> {
        let mut file = File::open(file).map_err(|e| ProgramParseError::FileError(Box::new(e)))?;
        let program = bincode::decode_from_std_read(
            &mut file,
            bincode::config::standard().with_fixed_int_encoding(),
        )
        .map_err(|e| ProgramParseError::BinaryDecodeError(Box::new(e)))?;
        Ok(program)
    }
}
impl std::ops::Index<Address> for Program {
    type Output = Instruction;

    fn index(&self, index: Address) -> &Self::Output {
        &self.0[index as usize]
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, bincode::Encode, bincode::Decode)]
pub enum Instruction {
    Sense {
        direction: RelativeDirection,
        condition: Condition,
        on_success: Address,
        on_failure: Address,
    },

    MarkPheromone(PheromoneIndex),
    UnmarkPheromone(PheromoneIndex),

    Pick(Address),
    Drop,

    TurnLeft,
    TurnRight,

    MoveForward(Address),
    MoveUp(Address),
    MoveDown(Address),
    Dig(Address),
    Fill(Address),
    DigUp(Address),
    DigDown(Address),
    FillUp,
    FillDown,

    Grab(Address),
    Attack(Address),

    Random {
        faces: u8,
        on_zero: Address,
        on_other: Address,
    },
    Goto(Address),
}
impl Instruction {
    pub fn is_jumping(&self) -> bool {
        matches!(
            self,
            Self::Sense { .. } | Self::Random { .. } | Self::Goto(_)
        )
    }

    pub fn cooldown(&self) -> u8 {
        match self {
            Self::Sense { .. }
            | Self::MarkPheromone(_)
            | Self::UnmarkPheromone(_)
            | Self::TurnLeft
            | Self::TurnRight
            | Self::Random { .. }
            | Self::Goto(_) => 1,
            Self::Pick(_) | Self::Drop => 5,
            Self::MoveForward(_) | Self::MoveUp(_) | Self::MoveDown(_) => 20,
            Self::Dig(_)
            | Self::Fill(_)
            | Self::DigUp(_)
            | Self::DigDown(_)
            | Self::FillUp
            | Self::FillDown => 25,
            Self::Grab(_) | Self::Attack(_) => 30,
        }
    }
}
impl TryFrom<(&str, &HashMap<&str, Address>)> for Instruction {
    type Error = InstructionParseError;

    fn try_from(
        (instruction, labels): (&str, &HashMap<&str, Address>),
    ) -> Result<Self, Self::Error> {
        fn get_label<'a>(
            tokens: &mut impl Iterator<Item = &'a str>,
            labels: &HashMap<&str, Address>,
        ) -> Result<Address, InstructionParseError> {
            let label = tokens
                .next()
                .ok_or(InstructionParseError::MissingArgument)?;
            labels
                .get(label)
                .copied()
                .ok_or(InstructionParseError::UndefinedLabel(label.into()))
        }
        fn get_argument<'a, T: std::str::FromStr>(
            tokens: &mut impl Iterator<Item = &'a str>,
        ) -> Result<T, InstructionParseError> {
            tokens
                .next()
                .ok_or(InstructionParseError::MissingArgument)?
                .parse()
                .map_err(|_| InstructionParseError::InvalidArgument)
        }

        let lowercase = instruction.to_lowercase();
        let mut tokens = lowercase.splitn(5, ' ');
        Ok(match tokens.next().ok_or(Self::Error::EmptyInstruction)? {
            "sense" => Self::Sense {
                direction: get_argument(&mut tokens)?,
                on_success: get_label(&mut tokens, labels)?,
                on_failure: get_label(&mut tokens, labels)?,
                condition: get_argument(&mut tokens)?,
            },
            "mark" => Self::MarkPheromone({
                let i = get_argument(&mut tokens)?;
                if i < 8 {
                    i
                } else {
                    return Err(Self::Error::InvalidArgument);
                }
            }),
            "unmark" => Self::UnmarkPheromone({
                let i = get_argument(&mut tokens)?;
                if i < 8 {
                    i
                } else {
                    return Err(Self::Error::InvalidArgument);
                }
            }),
            "pickup" => Self::Pick(get_label(&mut tokens, labels)?),
            "drop" => Self::Drop,
            "turnleft" => Self::TurnLeft,
            "turnright" => Self::TurnRight,
            "move" => Self::MoveForward(get_label(&mut tokens, labels)?),
            "moveup" => Self::MoveUp(get_label(&mut tokens, labels)?),
            "movedown" => Self::MoveDown(get_label(&mut tokens, labels)?),
            "dig" => Self::Dig(get_label(&mut tokens, labels)?),
            "fill" => Self::Fill(get_label(&mut tokens, labels)?),
            "digup" => Self::DigUp(get_label(&mut tokens, labels)?),
            "digdown" => Self::DigDown(get_label(&mut tokens, labels)?),
            "fillup" => Self::FillUp,
            "filldown" => Self::FillDown,
            "grab" => Self::Grab(get_label(&mut tokens, labels)?),
            "attack" => Self::Grab(get_label(&mut tokens, labels)?),
            "roll" => Self::Random {
                faces: get_argument(&mut tokens)?,
                on_zero: get_label(&mut tokens, labels)?,
                on_other: get_label(&mut tokens, labels)?,
            },
            "goto" => Self::Goto(get_label(&mut tokens, labels)?),
            s => return Err(Self::Error::InvalidInstruction(s.into())),
        })
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, bincode::Encode, bincode::Decode)]
pub enum RelativeDirection {
    Here,
    Ahead,
    Left,
    Right,
    Above,
    Below,
}
impl std::str::FromStr for RelativeDirection {
    type Err = ();

    fn from_str(token: &str) -> Result<Self, Self::Err> {
        Ok(match token {
            "here" => Self::Here,
            "ahead" => Self::Ahead,
            "leftahead" => Self::Left,
            "rightahead" => Self::Right,
            "above" => Self::Above,
            "below" => Self::Below,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, bincode::Encode, bincode::Decode)]
pub enum Condition {
    Friend,
    Enemy,
    Grabbed,

    FriendWithFood,
    EnemyWithFood,

    Food,
    Rock,
    Empty,
    Underground,
    Surface,
    HoleUp,
    HoleDown,

    Marker(PheromoneIndex),
    EnemyMarkers,

    Home,
    EnemyHome,
}
impl std::str::FromStr for Condition {
    type Err = ();

    fn from_str(token: &str) -> Result<Self, Self::Err> {
        let mut tokens = token.split_whitespace();
        Ok(match tokens.next().ok_or(())? {
            "friend" => Self::Friend,
            "enemy" => Self::Enemy,
            "grabbed" => Self::Grabbed,
            "friendwithfood" => Self::FriendWithFood,
            "enemywithfood" => Self::EnemyWithFood,
            "food" => Self::Food,
            "rock" => Self::Rock,
            "empty" => Self::Empty,
            "underground" => Self::Underground,
            "surface" => Self::Surface,
            "holeabove" => Self::HoleUp,
            "holebelow" => Self::HoleDown,
            "marker" => Self::Marker({
                let i = tokens.next().ok_or(())?.parse().map_err(|_| ())?;
                if i < 8 {
                    i
                } else {
                    return Err(());
                }
            }),
            "enemymarker" => Self::EnemyMarkers,
            "home" => Self::Home,
            "enemyhome" => Self::EnemyHome,
            _ => return Err(()),
        })
    }
}
