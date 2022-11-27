use std::num::{ParseFloatError, ParseIntError};

use super::location::Location;
use num_bigint::ParseBigIntError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RugsError {
    #[error("Parsing error: {} at {}.", msg, loc)]
    Parse { msg: String, loc: Location },
}

pub type Result<T> = std::result::Result<T, RugsError>;

impl From<ParseIntError> for RugsError {
    fn from(value: ParseIntError) -> Self {
        RugsError::Parse {
            msg: "Failed to parse int".to_string(),
            loc: Location::Unlocated,
        }
    }
}

impl From<ParseBigIntError> for RugsError {
    fn from(value: ParseBigIntError) -> Self {
        RugsError::Parse {
            msg: "Failed to parse bigint".to_string(),
            loc: Location::Unlocated,
        }
    }
}

impl From<ParseFloatError> for RugsError {
    fn from(value: ParseFloatError) -> Self {
        RugsError::Parse {
            msg: "Failed to parse float".to_string(),
            loc: Location::Unlocated,
        }
    }
}
