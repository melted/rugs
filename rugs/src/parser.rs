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

use log::info;

use self::lexing::{Token, TokenValue};
use crate::ast::{AstMaker, Metadata, Module, NodeId, TopExpression};
use crate::support::error;
use crate::support::{error::RugsError, location::Location};

pub fn parse(file_name: Option<&str>, code: &str) -> error::Result<Module> {
    info!("Parsing module from {}", file_name.unwrap_or("unnamed"));
    let mut state = ParserState::new(code);
    state.metadata.file = file_name.map(|f| f.to_string());
    let mut module = state.parse_module()?;
    module.metadata = state.metadata;
    Ok(module)
}

pub fn parse_expression(expr: &str) -> error::Result<TopExpression> {
    info!("Parsing expression");
    let mut state = ParserState::new(expr);
    state.layout_start = false;
    let exp = state.parse_expression()?;
    Ok(TopExpression {
        metadata: state.metadata,
        expression: exp,
    })
}

pub fn dump_tokens(code: &str, output: &mut impl Write) -> error::Result<()> {
    let mut state = ParserState::new(code);
    info!("Dumping tokens from lexer");
    loop {
        let t = state.get_next_token()?;
        writeln!(output, "{:?}", t).unwrap(); // YOLO
        if t.value == TokenValue::Eof {
            break;
        }
    }
    Ok(())
}

pub fn dump_ast(code: &str, output: &mut impl Write) -> error::Result<()> {
    let module = parse(None, code)?;
    info!("Dumping module AST");
    writeln!(output, "{:?}", module)?;
    Ok(())
}

pub fn dump_ast_expression(code: &str, output: &mut impl Write) -> error::Result<()> {
    let exp = parse_expression(code)?;
    info!("Dumping expression AST");
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

    pub(self) fn error(&self, msg: &str) -> RugsError {
        RugsError::Parse {
            msg: msg.to_string(),
            loc: Location::Offset {
                start: self.pos,
                end: self.pos,
            },
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
