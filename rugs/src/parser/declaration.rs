use crate::{ast::Declaration};
use super::{ParserState, lexing::TokenValue, helpers::error};

impl<'a> ParserState<'a> {

    pub (super) fn parse_declaration(&mut self) -> anyhow::Result<Declaration> {
        unimplemented!()
    }
}