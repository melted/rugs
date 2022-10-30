use crate::ast::Declaration;
use super::{ParserState, ParseError};

impl<'a> ParserState<'a> {
    pub (super) fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        unimplemented!()
    }
}