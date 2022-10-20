mod tests;
mod lexing;

use crate::ast::Module;

pub (self) struct ParserState<'a> {

}

impl<'a> ParserState<'a> {
    fn new() -> ParserState<'a> {
        ParserState {  }
    }
}

pub struct ParseError {
    msg : String,
    loc : (usize, usize)
}

impl ParseError {
    pub fn new(what : &str, loc : (usize, usize)) -> ParseError {
        ParseError { msg: what.to_string(), loc }
    }
}

pub fn parse(code : &str) -> Result<Module, ParseError> {
    let state = ParserState::new();

    Err(ParseError::new("bah", (0,0)))
}

