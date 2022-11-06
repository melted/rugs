use crate::ast::Pattern;

use super::{ParserState};


impl<'a> ParserState<'a> {
    pub (super) fn parse_pattern(&mut self) -> anyhow::Result<Pattern> {
        unimplemented!()
    }

    
    fn parse_lpattern(&mut self) -> anyhow::Result<Pattern> {
        unimplemented!()
    }

    
    pub (super) fn parse_apattern(&mut self) -> anyhow::Result<Pattern> {
        unimplemented!()
    }
}