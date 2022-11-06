use thiserror::Error;
use crate::location::Location;

#[derive(Error, Debug)]
pub enum RugsError {
    #[error("Parsing error: {} at {}.", msg, loc)]
    Parse {
        msg: String,
        loc: Location
    }
}