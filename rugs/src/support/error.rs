use super::location::Location;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RugsError {
    #[error("Parsing error: {} at {}.", msg, loc)]
    Parse { msg: String, loc: Location },
}
