mod declaration;
mod expression;
mod helpers;
mod lexing;
mod module;
mod pattern;
mod tests;
mod types;

use std::collections::VecDeque;
use std::io::Write;
use std::iter::Peekable;
use std::str::CharIndices;


use crate::ast::{Module, Expression, AstMaker, NodeId};
use crate::error::RugsError;
use crate::location::Location;
use self::lexing::{Token, TokenValue};

pub fn parse(file_name : Option<&str>, code : &str) -> anyhow::Result<Module> {
    let mut state = ParserState::new(code);
    state.file_name = file_name;
    let module = state.parse_module()?;
    Ok(module)
}

pub fn parse_expression(expr : &str) -> anyhow::Result<Expression> {
    let mut state = ParserState::new(expr);
    state.parse_expression()
}

pub fn dump_tokens(code : &str, output : &mut impl Write) -> anyhow::Result<()> {
    let mut state = ParserState::new(code);

    loop {
        let t = state.get_next_token()?;
        writeln!(output, "{:?}", t).unwrap(); // YOLO
        if t.value == TokenValue::Eof { break; }
    }
    Ok(())
}

#[derive(Debug)]
pub (self) struct ParserState<'a> {
    file_name : Option<&'a str>,
    src : &'a str,
    chars : Peekable<CharIndices<'a>>,
    queue : VecDeque<Token>,
    pushed_back : VecDeque<Token>,
    newlines : Vec<usize>,
    pos : usize,
    token_start : usize,
    layout_stack : Vec<usize>,
    layout_start : bool,
    indent : Option<usize>,
    counter : NodeId
}

impl<'a> ParserState<'a> {
    fn new(code : &'a str) -> ParserState<'a> {
        ParserState {
            file_name: None,
            src: code,
            chars: code.char_indices().peekable(),
            queue: VecDeque::new(),
            pushed_back: VecDeque::new(),
            newlines: Vec::new(),
            pos: 0,
            token_start: 0,
            layout_stack: Vec::new(),
            layout_start: true,
            indent: None,
            counter: 0
        }
    }
}

impl<'a> AstMaker for ParserState<'a> {
    fn next_id(&mut self) -> NodeId {
        let val = self.counter;
        self.counter += 1;
        val
    }
}
