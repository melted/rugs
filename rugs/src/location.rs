use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
    Offset { start : usize, end : usize },
    Unlocated
}

#[derive(Debug, Clone, PartialEq)]
pub struct FileLocation {  
    file : String,
    start : (usize, usize),
    end : (usize, usize)
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Offset { start, end } => {
                write!(f, "offset {}-{}", start, end)?;
                Ok(())
            },
            Location::Unlocated => {
                write!(f, "unknown location")?;
                Ok(())
            }
        }
    }
}

impl Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start.0 == self.end.0 {
            write!(f, "file {}: line {}:{}-{}", self.file, self.start.0, self.start.1, self.end.1)?;
        } else {
            write!(f, "file {}: lines {}:{}-{}:{}", self.file, self.start.0, self.start.1, self.end.0, self.end.1)?;
        }
        Ok(())
    }
}