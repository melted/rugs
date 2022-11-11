use crate::{error::RugsError, location::Location, ast::Identifier};

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

    pub (super) fn peek_next(&mut self, t : TokenValue) -> anyhow::Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub (super) fn optional_token(&mut self, t : TokenValue) -> anyhow::Result<()> {
        self.is_next(t)?;
        Ok(())
    }

    pub (super) fn expect_token_value(&mut self, t : &mut Token) -> anyhow::Result<()> {
        let success = self.optional_token_value(t)?;
        if !success {
            let tok = self.get_next_token()?;
            error("bad token type", tok.location)
        } else {
            Ok(())
        }
    }

    pub (super) fn optional_token_value(&mut self, t : &mut Token) -> anyhow::Result<bool> {
        let next = self.peek_next_token()?;
        if next.same_token_type(t) {
            let tok = self.get_next_token()?;
            t.value = tok.value;
            t.location = tok.location;
            Ok(true)
        } else {
            Ok(false)
        }
    }


    pub (super) fn optional_semicolon(&mut self) -> anyhow::Result<()> {
        self.optional_token(TokenValue::Semicolon)?;
        self.optional_token(TokenValue::VirtualSemicolon)
    }

    pub (super) fn try_parse<T>(&mut self, 
                                mut inner_parser : impl FnMut(&mut Self) -> anyhow::Result<T>) -> anyhow::Result<Option<T>> {
        let start = self.consumed_tokens.len();
        match inner_parser(self) {
            Ok(res) => Ok(Some(res)),
            Err(_) => {
                let tokens_used = self.consumed_tokens.len() - start;
                self.rewind_lexer(tokens_used);
                Ok(None)
            }
        }
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

    pub (super) fn parse_qvar(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarId(_, _) | TokenValue::VarId(_) => Ok(Identifier::try_from(tok)?),
            TokenValue::LeftParen => {
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::QVarSym(_, _) | TokenValue::VarSym(_) => {
                        self.expect(TokenValue::RightParen)?;
                        Ok(Identifier::try_from(tok)?)
                    },
                    _ => return error("expected varid", tok.location)
                }
            },
            _ => error("Expected qvar", tok.location)
        }
    }

    pub (super) fn parse_qcon(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QConId(_, _) | TokenValue::ConId(_) => Ok(Identifier::try_from(tok)?),
            TokenValue::LeftParen => {
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::QConSym(_, _) | TokenValue::ConSym(_) => {
                        self.expect(TokenValue::RightParen)?;
                        Ok(Identifier::try_from(tok)?)
                    },
                    _ => return error("expected varid", tok.location)
                }
            },
            _ => error("Expected qvar", tok.location)
        }
    }

    pub (super) fn parse_qvarcon(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarId(_, _) | TokenValue::VarId(_) 
            | TokenValue::QConId(_, _) | TokenValue::ConId(_)
                => Ok(Identifier::try_from(tok)?),
            TokenValue::LeftParen => {
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::QVarSym(_, _) | TokenValue::VarSym(_)
                    | TokenValue::QConSym(_, _) | TokenValue::ConSym(_) => {
                        self.expect(TokenValue::RightParen)?;
                        Ok(Identifier::try_from(tok)?)
                    },
                    _ => return error("expected varid", tok.location)
                }
            },
            _ => error("Expected qvar", tok.location)
        }
    }

    pub (super) fn parse_qvarsym(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarSym(_, _) | TokenValue::VarSym(_) => Ok(Identifier::try_from(tok)?),
            TokenValue::Backtick => {
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::QVarId(_, _) | TokenValue::VarId(_) => {
                        self.expect(TokenValue::Backtick)?;
                        Ok(Identifier::try_from(tok)?)
                    },
                    _ => return error("expected varid", tok.location)
                }
            },
            _ => error("Expected qvar", tok.location)
        }
    }
}

pub (super) fn error<T>(msg : &str, loc : Location) -> anyhow::Result<T>{
    Err(RugsError::Parse { msg: msg.to_string(), loc: loc }.into())
}
