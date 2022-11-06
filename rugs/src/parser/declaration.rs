use crate::{ast::Declaration};
use super::{ParserState, lexing::TokenValue, helpers::error};

impl<'a> ParserState<'a> {
    pub (super) fn parse_declarations<T>(&mut self,
                                   mut inner_parser : impl FnMut(&mut Self, bool) -> anyhow::Result<T>) 
                                   -> anyhow::Result<Vec<T>> {
        let mut output : Vec<T> = Vec::new();
        let brace = self.get_next_token()?;
        let is_virtual = match brace.value {
            TokenValue::LeftBrace => false,
            TokenValue::VirtualLeftBrace => true,
            _ => return error("Expected left brace after let", brace.location)
        };
        if !((!is_virtual && self.is_next(TokenValue::RightBrace)?) ||
            (is_virtual && self.is_next(TokenValue::VirtualRightBrace)?))  {
            loop {
                let res = inner_parser(self, is_virtual)?;
                output.push(res);
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::Semicolon | TokenValue::VirtualSemicolon => {},
                    TokenValue::RightBrace if !is_virtual => break,
                    TokenValue::VirtualRightBrace if is_virtual => break,
                    _ => return error("Unexpected token in let", tok.location)
                }
            }
        }
        Ok(output)
    }

    pub (super) fn parse_declaration(&mut self) -> anyhow::Result<Declaration> {
        unimplemented!()
    }
}