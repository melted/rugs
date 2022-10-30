mod declaration;
mod expression;
mod helpers;
mod lexing;
mod pattern;
mod tests;
mod types;

use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};
use std::io::Write;
use std::iter::Peekable;
use std::str::CharIndices;

use num_bigint::ParseBigIntError;

use crate::ast::{Annotated, Module, Expression};

use self::lexing::Token;

pub fn parse(code : &str) -> Result<Module, ParseError> {
    let mut state = ParserState::new(code);

    Err(ParseError::new("bah", None))
}

pub fn parse_expression(expr : &str) -> Result<Expression, ParseError> {
    let mut state = ParserState::new(expr);
    state.parse_expression()
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
    pushed_back : VecDeque<Annotated<Token>>,
    newlines : Vec<usize>,
    pos : usize,
    token_start : usize,
    layout_stack : Vec<usize>,
    layout_start : bool,
    indent : Option<usize>,
}

impl<'a> ParserState<'a> {
    fn new(code : &'a str) -> ParserState<'a> {
        ParserState { 
            src: code,
            chars: code.char_indices().peekable(),
            queue: VecDeque::new(),
            pushed_back: VecDeque::new(),
            newlines: Vec::new(),
            pos: 0,
            token_start: 0,
            layout_stack: Vec::new(),
            layout_start: true,
            indent: None
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    msg : String,
    loc : Option<(usize, usize)>
}

// TODO: A renderer for ParseError that shows the location by row and col.
impl ParseError {
    pub fn new(what : &str, loc : Option<(usize, usize)>) -> ParseError {
        ParseError { msg: what.to_string(), loc }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.loc {
            Some((start, end)) => 
                f.write_fmt(format_args!("Syntax error: {} at pos {}-{}", self.msg, start, end))?,
            None =>
                f.write_fmt(format_args!("Syntax error: {}, no location available", self.msg))?
        }
        Ok(())
    }
}

impl Error for ParseError {

}
// TODO: Using these loses the position, so the errors should be
// intercepted at the call site and these removed.
impl From<ParseFloatError> for ParseError {
    fn from(value: ParseFloatError) -> Self {
        ParseError { msg: value.to_string(), loc: None }
    }
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        ParseError { msg: value.to_string(), loc: None }
    }
}

impl From<ParseBigIntError> for ParseError {
    fn from(value: ParseBigIntError) -> Self {
        ParseError { msg: value.to_string(), loc: None }
    }
}
