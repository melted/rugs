mod tests;
mod lexing;

use std::num::{ParseFloatError, ParseIntError};
use std::{collections::VecDeque, io::Write};
use std::iter::Peekable;
use std::str::CharIndices;

use num_bigint::ParseBigIntError;

use crate::ast::{Annotated, Module, Expression};

use self::lexing::Token;

pub fn parse(code : &str) -> Result<Module, ParseError> {
    let mut state = ParserState::new(code);

    Err(ParseError::new("bah", (0,0)))
}

pub fn parse_expression(expr : &str) -> Result<Expression, ParseError> {
    let mut state = ParserState::new(expr);

    Err(ParseError::new("bah", (0,0)))
}

pub fn dump_tokens(code : &str, output : &mut impl Write) -> Result<(), ParseError> {
    let mut state = ParserState::new(code);

    loop {
        let t = state.get_next_token()?;
        writeln!(output, "{:?}", t).unwrap(); // YOLO
        if *t == Token::Eof { break; }
    }

    Ok(())
}

#[derive(Debug)]
pub (self) struct ParserState<'a> {
    src : &'a str,
    chars : Peekable<CharIndices<'a>>,
    queue : VecDeque<Annotated<Token>>,
    newlines : Vec<usize>,
    pos : usize,
    token_start : usize,
    column : usize
}

impl<'a> ParserState<'a> {
    fn new(code : &'a str) -> ParserState<'a> {
        ParserState { 
            src: code,
            chars: code.char_indices().peekable(),
            queue: VecDeque::new(),
            newlines: Vec::new(),
            pos: 0,
            token_start: 0,
            column: 0
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    msg : String,
    loc : (usize, usize)
}

// TODO: A renderer for ParseError that shows the location by row and col.
impl ParseError {
    pub fn new(what : &str, loc : (usize, usize)) -> ParseError {
        ParseError { msg: what.to_string(), loc }
    }
}

// TODO: Using these loses the position, so the errors should be
// intercepted at the call site and these removed.
impl From<ParseFloatError> for ParseError {
    fn from(value: ParseFloatError) -> Self {
        ParseError { msg: value.to_string(), loc: (0,0) }
    }
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        ParseError { msg: value.to_string(), loc: (0,0) }
    }
}

impl From<ParseBigIntError> for ParseError {
    fn from(value: ParseBigIntError) -> Self {
        ParseError { msg: value.to_string(), loc: (0,0) }
    }
}