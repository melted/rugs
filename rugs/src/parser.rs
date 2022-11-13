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


use crate::ast::{Module, Expression, AstMaker, NodeId, Metadata, TopExpression};
use crate::error::RugsError;
use crate::location::Location;
use self::lexing::{Token, TokenValue};

pub fn parse(file_name : Option<&str>, code : &str) -> anyhow::Result<Module> {
    let mut state = ParserState::new(code);
    state.metadata.file = file_name.map(|f| f.to_string());
    let mut module = state.parse_module()?;
    module.metadata = state.metadata;
    Ok(module)
}

pub fn parse_expression(expr : &str) -> anyhow::Result<TopExpression> {
    let mut state = ParserState::new(expr);
    let exp = state.parse_expression()?;
    Ok(TopExpression { metadata: state.metadata, expression: exp })
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
    metadata : Metadata,
    src : &'a str,
    chars : Peekable<CharIndices<'a>>,
    queue : VecDeque<Token>,
    pushed_back : VecDeque<Token>,
    consumed_tokens : Vec<Token>,
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
            metadata: Metadata::new(),
            src: code,
            chars: code.char_indices().peekable(),
            queue: VecDeque::new(),
            pushed_back: VecDeque::new(),
            consumed_tokens: Vec::new(),
            pos: 0,
            token_start: 0,
            layout_stack: Vec::new(),
            layout_start: true,
            indent: None,
            counter: 0
        }
    }

    pub (self) fn error(&self, msg : &str) -> anyhow::Error {
        RugsError::Parse { msg: msg.to_string(),
             loc: Location::Offset { start: self.pos, end: self.pos } }.into()
    }
}

impl<'a> AstMaker for ParserState<'a> {
    fn next_id(&mut self) -> NodeId {
        let val = self.counter;
        self.counter += 1;
        val
    }
}
