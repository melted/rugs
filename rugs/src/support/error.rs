use super::location::Location;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RugsError {
    #[error("Parsing error: {} at {}.", msg, loc)]
    Parse { msg: String, loc: Location },
    #[error("IO error: {}", err)]
    IO { err: std::io::Error }
}

pub type Result<T> = std::result::Result<T, RugsError>;

impl From<std::io::Error> for RugsError {
    fn from(value: std::io::Error) -> Self {
        RugsError::IO { err: value }
    }
}