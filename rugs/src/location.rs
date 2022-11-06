use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Offset { start : usize, end : usize },
    InContext {
        file : String,
        start : (usize, usize),
        end : (usize, usize)
    },
    Unlocated
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Offset { start, end } => {
                write!(f, "offset {}-{}", start, end)?;
                Ok(())
            },
            Location::InContext { file, start, end } => {
                if start.0 == end.0 {
                    write!(f, "file {}: line {}:{}-{}", file, start.0, start.1, end.1)?;
                } else {
                    write!(f, "file {}: lines {}:{}-{}:{}", file, start.0, start.1, end.0, end.1)?;
                }
                Ok(())
            },
            Location::Unlocated => {
                write!(f, "unknown location")?;
                Ok(())
            }
        }
    }
}