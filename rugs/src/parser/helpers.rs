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
}

pub (super) fn error<T>(msg : &str, loc : Location) -> anyhow::Result<T>{
    Err(RugsError::Parse { msg: msg.to_string(), loc: loc }.into())
}