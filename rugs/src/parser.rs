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

use self::lexing::{Token, TokenValue};
use crate::ast::{AstMaker, Expression, Metadata, Module, NodeId, TopExpression};
use crate::error::RugsError;
use crate::location::Location;

pub fn parse(file_name: Option<&str>, code: &str) -> anyhow::Result<Module> {
    let mut state = ParserState::new(code);
    state.metadata.file = file_name.map(|f| f.to_string());
    let mut module = state.parse_module()?;
    module.metadata = state.metadata;
    Ok(module)
}

pub fn parse_expression(expr: &str) -> anyhow::Result<TopExpression> {
    let mut state = ParserState::new(expr);
    state.layout_start = false;
    let exp = state.parse_expression()?;
    Ok(TopExpression {
        metadata: state.metadata,
        expression: exp,
    })
}

pub fn dump_tokens(code: &str, output: &mut impl Write) -> anyhow::Result<()> {
    let mut state = ParserState::new(code);

    loop {
        let t = state.get_next_token()?;
        writeln!(output, "{:?}", t).unwrap(); // YOLO
        if t.value == TokenValue::Eof {
            break;
        }
    }
    Ok(())
}

pub fn dump_ast(code: &str, output: &mut impl Write) -> anyhow::Result<()> {
    let module = parse(None, code)?;
    writeln!(output, "{:?}", module)?;
    Ok(())
}

pub fn dump_ast_expression(code: &str, output: &mut impl Write) -> anyhow::Result<()> {
    let exp = parse_expression(code)?;
    writeln!(output, "{:?}", exp)?;
    Ok(())
}

#[derive(Debug)]
pub(self) struct ParserState<'a> {
    metadata: Metadata,
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    queue: VecDeque<Token>,
    tokens: Vec<Token>,
    token_pos: usize,
    pos: usize,
    token_start: usize,
    layout_stack: Vec<usize>,
    layout_start: bool,
    indent: Option<usize>,
    counter: NodeId,
}

impl<'a> ParserState<'a> {
    fn new(code: &'a str) -> ParserState<'a> {
        ParserState {
            metadata: Metadata::new(),
            src: code,
            chars: code.char_indices().peekable(),
            queue: VecDeque::new(),
            tokens: Vec::new(),
            token_pos: 0,
            pos: 0,
            token_start: 0,
            layout_stack: vec![],
            layout_start: true,
            indent: None,
            counter: 0,
        }
    }

    pub(self) fn error(&self, msg: &str) -> anyhow::Error {
        //println!("{}", std::backtrace::Backtrace::capture());
        RugsError::Parse {
            msg: msg.to_string(),
            loc: Location::Offset {
                start: self.pos,
                end: self.pos,
            },
        }
        .into()
    }
}

impl<'a> AstMaker for ParserState<'a> {
    fn next_id(&mut self) -> NodeId {
        let val = self.counter;
        self.counter += 1;
        val
    }
}
