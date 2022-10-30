use crate::ast::Pattern;

use super::{ParserState, ParseError};


impl<'a> ParserState<'a> {
    pub (super) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }

    
    fn parse_lpattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }

    
    pub (super) fn parse_apattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }
}