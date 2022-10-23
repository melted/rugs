mod tests;
mod lexing;

use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::CharIndices;

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

#[derive(Debug)]
pub (self) struct ParserState<'a> {
    src : &'a str,
    chars : Peekable<CharIndices<'a>>,
    queue : VecDeque<Annotated<Token>>,
    newlines : Vec<usize>,
    pos : usize,
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
            column: 0
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    msg : String,
    loc : (usize, usize)
}

impl ParseError {
    pub fn new(what : &str, loc : (usize, usize)) -> ParseError {
        ParseError { msg: what.to_string(), loc }
    }
}
