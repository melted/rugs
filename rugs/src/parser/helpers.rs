use crate::{error::RugsError, location::Location, ast::*};

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

    pub (super) fn parse_some<T>(&mut self, 
                    inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>) -> 
                    anyhow::Result<Vec<T>> {
        let mut output = Vec::new();
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub (super) fn parse_first_of<T>(&mut self, 
                    parsers : Vec<&mut impl FnMut(&mut Self) -> anyhow::Result<T>>)
                     -> anyhow::Result<T> {
        for p in parsers {
            if let Some(res) = self.try_parse(p)? {
                return Ok(res);
            }
        }
        error("no alternative succeded", self.peek_next_token()?.location)
    }


    pub (super) fn parse_unit(&mut self) -> anyhow::Result<Identifier> {
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::RightParen)?;
        Ok(consym("()"))
    }

    pub (super) fn parse_empty_list(&mut self) -> anyhow::Result<Identifier> {
        self.expect(TokenValue::LeftBracket)?;
        self.expect(TokenValue::RightBracket)?;
        Ok(consym("[]"))
    }


    pub (super) fn parse_funcon(&mut self) -> anyhow::Result<Identifier> {
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::RightArrow)?;
        self.expect(TokenValue::RightParen)?;
        Ok(consym("(->)"))
    }

    pub (super) fn parse_tuplecon(&mut self) -> anyhow::Result<Identifier> {
        let mut str ="(,".to_string();
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::Comma)?;
        while self.is_next(TokenValue::Comma)? { 
            str.push(',')
        }
        self.expect(TokenValue::RightParen)?;
        str.push(')');
        Ok(consym(&str))
    }

    pub (super) fn parse_separated_by<T>(&mut self, 
                    inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>,
                    separator : TokenValue) -> anyhow::Result<Vec<T>> {
        let mut output = Vec::new();
        output.push(inner_parser(self)?);
        while self.is_next(separator.clone())? {
            output.push(inner_parser(self)?);
        }
        Ok(output)
    }



    pub (super) fn optional_semicolon(&mut self) -> anyhow::Result<()> {
        self.optional_token(TokenValue::Semicolon)?;
        self.optional_token(TokenValue::VirtualSemicolon)
    }

    pub (super) fn parse_conid(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConId(s) => Ok(conid(&s)),
            _ => error("expected a conid", tok.location)
        }
    }

    pub (super) fn parse_qconid(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConId(s) => Ok(conid(&s)),
            TokenValue::QConId(m,s) => Ok(qconid(&m,&s)),
            _ => error("expected a conid", tok.location)
        }
    }

    pub (super) fn parse_varid(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarId(s) => Ok(varid(&s)),
            _ => error("expected a varid", tok.location)
        }
    }

    pub (super) fn parse_qvarid(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarId(s) => Ok(varid(&s)),
            TokenValue::QVarId(m, s) => Ok(qvarid(&m,&s)),
            _ => error("expected a qualified varid", tok.location)
        }
    }

    pub (super) fn parse_varsym(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarSym(s) => Ok(varsym(&s)),
            _ => error("expected a varsym", tok.location)
        }
    }

    pub (super) fn parse_qvarsym(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarSym(s) => Ok(varsym(&s)),
            TokenValue::QVarSym(m, s) => Ok(qvarsym(&m,&s)),
            _ => error("expected a qualified operator", tok.location)
        }
    }

    
    pub (super) fn parse_consym(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConSym(s) => Ok(consym(&s)),
            _ => error("expected a constructor operator", tok.location)
        }
    }

    pub (super) fn parse_gconsym(&mut self) -> anyhow::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::Colon => Ok(consym(":")),
            TokenValue::ConSym(s) => Ok(consym(&s)),
            TokenValue::QConSym(m, s) => Ok(qconsym(&m,&s)),
            _ => error("expected a qualified constructor operator", tok.location)
        }
    }

    pub (super) fn parse_surrounded_parens<T>(&mut self,
                            inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>)
                            -> anyhow::Result<T> {
        self.parse_surrounded_by(TokenValue::LeftParen, inner_parser, TokenValue::RightParen)
    }

    pub (super) fn parse_surrounded_by<T>(&mut self,
                            before : TokenValue,
                            inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>,
                            after : TokenValue)
                            -> anyhow::Result<T> {
        self.expect(before)?;
        let val = inner_parser(self)?;
        self.expect(after)?;
        Ok(val)
    }

    pub (super) fn try_parse<T>(&mut self, 
                                inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>) -> anyhow::Result<Option<T>> {
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
                                   inner_parser : &mut impl FnMut(&mut Self, bool) -> anyhow::Result<T>) 
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
                                       inner_parser : &mut impl FnMut(&mut Self) -> anyhow::Result<T>)
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

    pub (super) fn parse_var(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_varid)?{
            Ok(res)
        } else {
            self.parse_surrounded_parens(&mut Self::parse_varsym).map_err(|_|
                self.error("expected identifier"))
        }
    }

    pub (super) fn parse_qvar(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qvarid)?{
            Ok(res)
        } else {
            self.parse_surrounded_parens(&mut Self::parse_qvarsym).map_err(|_|
                self.error("expected qualified identifier"))
        }
    }

    pub (super) fn parse_con(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_conid)?{
            Ok(res)
        } else {
            self.parse_surrounded_parens(&mut Self::parse_consym).map_err(|_|
                self.error("expected constructor"))
        }
    }

    pub (super) fn parse_qcon(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qconid)?{
            Ok(res)
        } else {
            self.parse_surrounded_parens(&mut Self::parse_gconsym).map_err(|_|
                self.error("expected qualified constructor"))
        }
    }

    pub (super) fn parse_cname(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_var)? {
            Ok(res)
        } else {
            self.parse_con().map_err(|_|
                self.error("expected variable or constructor"))
        }
    }

    pub (super) fn parse_varop(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qvarsym)? {
            Ok(res)
        } else {
            self.parse_surrounded_by(TokenValue::Backtick, &mut Self::parse_varid, TokenValue::Backtick).map_err(|_|
                self.error("expected operator"))
        }
    }

    pub (super) fn parse_qvarop(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qvarsym)? {
            Ok(res)
        } else {
            self.parse_surrounded_by(TokenValue::Backtick, &mut Self::parse_qvarid, TokenValue::Backtick).map_err(|_|
                self.error("expected qualified operator"))
        }
    }

    pub (super) fn parse_conop(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_consym)? {
            Ok(res)
        } else {
            self.parse_surrounded_by(TokenValue::Backtick, &mut Self::parse_con, TokenValue::Backtick).map_err(|_|
                self.error("expected constructor operator"))
        }
    }

    pub (super) fn parse_qconop(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_gconsym)? {
            Ok(res)
        } else {
            self.parse_surrounded_by(TokenValue::Backtick, &mut Self::parse_qcon, TokenValue::Backtick).map_err(|_|
                self.error("expected qualified constructor operator"))
        }
    }

    pub (super) fn parse_op(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_varop)? {
            Ok(res)
        } else {
            self.parse_conop().map_err(|_|
                self.error("expected an operator"))
        }
    }

    pub (super) fn parse_qop(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qvarop)? {
            Ok(res)
        } else {
            self.parse_qconop().map_err(|_|
                self.error("expected an operator"))
        }
    }

    pub (super) fn parse_gcon(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_unit)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_empty_list)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_tuplecon)? {
            Ok(res)
        } else {
            self.parse_qcon().map_err(|_| self.error("Not a gcon"))
        }
    }
}

pub (super) fn error<T>(msg : &str, loc : Location) -> anyhow::Result<T>{
    Err(RugsError::Parse { msg: msg.to_string(), loc: loc }.into())
}
