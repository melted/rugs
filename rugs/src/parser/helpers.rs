use crate::{error::RugsError, location::Location};

use super::{lexing::{Token, TokenValue}, ParserState};


impl<'a> ParserState<'a> {
    pub (super) fn expect(&mut self, t : TokenValue) -> anyhow::Result<()> {
        let next = self.get_next_token()?;
        if t != next.value {
            error("expected {}, got {}", next.location) // TODO: Fix error location
        } else {
            Ok(())
        }
    }

    pub (super) fn is_next(&mut self, t : TokenValue) -> anyhow::Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            self.get_next_token()?; // Swallow token
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub (super) fn optional_token(&mut self, t : TokenValue) -> anyhow::Result<()> {
        self.is_next(t)?;
        Ok(())
    }

    pub (super) fn optional_semicolon(&mut self) -> anyhow::Result<()> {
        self.optional_token(TokenValue::Semicolon)?;
        self.optional_token(TokenValue::VirtualSemicolon)
    }

    pub (super) fn parse_braced_list<T>(&mut self,
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

    pub (super) fn parse_paren_list<T>(&mut self,
                                       mut inner_parser : impl FnMut(&mut Self) -> anyhow::Result<T>)
                                        -> anyhow::Result<Vec<T>> {
        let mut output = Vec::new();
        self.expect(TokenValue::LeftParen)?;
        loop {
            if self.is_next(TokenValue::RightParen)? {
                break;
            }
            let res = inner_parser(self)?;
            output.push(res);
            let tok = self.get_next_token()?;
            match  tok.value {
                TokenValue::Comma => {},
                TokenValue::RightParen => break,
                _ => return error("Expected ',' or ')' in export list", tok.location) 
            }
        }
        Ok(output)
    }
}

pub (super) fn error<T>(msg : &str, loc : Location) -> anyhow::Result<T>{
    Err(RugsError::Parse { msg: msg.to_string(), loc: loc }.into())
}