use crate::{
    ast::*,
    support::{
        error::{self, RugsError},
        location::Location,
    },
};

use super::{lexing::TokenValue, ParserState};

impl<'a> ParserState<'a> {
    pub(super) fn expect(&mut self, t: TokenValue) -> error::Result<()> {
        let next = self.get_next_token()?;
        if t != next.value {
            Err(self.error(&format!("expected {:?}, got {:?}", t, next.value)))
        } else {
            Ok(())
        }
    }

    pub(super) fn is_next(&mut self, t: TokenValue) -> error::Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            self.get_next_token()?; // Swallow token
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn peek_next(&mut self, t: TokenValue) -> error::Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn optional_token(&mut self, t: TokenValue) -> error::Result<()> {
        self.is_next(t)?;
        Ok(())
    }

    pub(super) fn parse_some<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
    ) -> error::Result<Vec<T>> {
        let mut output = Vec::new();
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_some1<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
    ) -> error::Result<Vec<T>> {
        let mut output = Vec::new();
        let first = inner_parser(self)?;
        output.push(first);
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_none<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
    ) -> error::Result<()> {
        match self.try_parse(inner_parser)? {
            Some(_) => Err(self.error("Expected none")),
            None => Ok(()),
        }
    }

    pub(super) fn parse_unit(&mut self) -> error::Result<Identifier> {
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::RightParen)?;
        Ok(conid("()"))
    }

    pub(super) fn parse_empty_list(&mut self) -> error::Result<Identifier> {
        self.expect(TokenValue::LeftBracket)?;
        self.expect(TokenValue::RightBracket)?;
        Ok(conid("[]"))
    }

    pub(super) fn parse_funcon(&mut self) -> error::Result<Identifier> {
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::RightArrow)?;
        self.expect(TokenValue::RightParen)?;
        Ok(conid("(->)"))
    }

    pub(super) fn parse_tuplecon(&mut self) -> error::Result<Identifier> {
        let mut str = "(,".to_string();
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::Comma)?;
        while self.is_next(TokenValue::Comma)? {
            str.push(',')
        }
        self.expect(TokenValue::RightParen)?;
        str.push(')');
        Ok(conid(&str))
    }

    pub(super) fn parse_separated_by<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
        separator: TokenValue,
    ) -> error::Result<Vec<T>> {
        let mut output = Vec::new();
        output.push(inner_parser(self)?);
        while self.is_next(separator.clone())? {
            output.push(inner_parser(self)?);
        }
        Ok(output)
    }

    pub(super) fn optional_semicolon(&mut self) -> error::Result<()> {
        self.optional_token(TokenValue::Semicolon)
    }

    pub(super) fn parse_conid(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConId(s) => Ok(conid(&s)),
            _ => Err(self.error("expected a conid")),
        }
    }

    pub(super) fn parse_qconid(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConId(s) => Ok(conid(&s)),
            TokenValue::QConId(m, s) => Ok(qconid(&m, &s)),
            _ => Err(self.error("expected a conid")),
        }
    }

    pub(super) fn parse_varid(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarId(s) => Ok(varid(&s)),
            _ => Err(self.error("expected a varid")),
        }
    }

    pub(super) fn parse_word(&mut self) -> error::Result<String> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarId(s) => Ok(s),
            TokenValue::ConId(s) => Ok(s),
            _ => Err(self.error("expected a varid")),
        }
    }

    pub(super) fn parse_qvarid(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarId(s) => Ok(varid(&s)),
            TokenValue::QVarId(m, s) => Ok(qvarid(&m, &s)),
            _ => Err(self.error("expected a qualified varid")),
        }
    }

    pub(super) fn parse_varsym(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarSym(s) => Ok(varsym(&s)),
            _ => Err(self.error("expected a varsym")),
        }
    }

    pub(super) fn parse_qvarsym(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::VarSym(s) => Ok(varsym(&s)),
            TokenValue::QVarSym(m, s) => Ok(qvarsym(&m, &s)),
            _ => Err(self.error("expected a qualified operator")),
        }
    }

    pub(super) fn parse_consym(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::ConSym(s) => Ok(consym(&s)),
            _ => Err(self.error("expected a constructor operator")),
        }
    }

    pub(super) fn parse_gconsym(&mut self) -> error::Result<Identifier> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::Colon => Ok(consym(":")),
            TokenValue::ConSym(s) => Ok(consym(&s)),
            TokenValue::QConSym(m, s) => Ok(qconsym(&m, &s)),
            _ => Err(self.error("expected a qualified constructor operator")),
        }
    }

    pub(super) fn parse_surrounded_by<T>(
        &mut self,
        before: TokenValue,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
        after: TokenValue,
    ) -> error::Result<T> {
        self.expect(before)?;
        let val = inner_parser(self)?;
        self.expect(after)?;
        Ok(val)
    }

    pub(super) fn try_parse<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
    ) -> error::Result<Option<T>> {
        let start = self.token_pos;
        match inner_parser(self) {
            Ok(res) => Ok(Some(res)),
            Err(_err) => {
                self.token_pos = start;
                Ok(None)
            }
        }
    }

    pub(super) fn parse_braced_list<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self, bool) -> error::Result<T>,
    ) -> error::Result<Vec<T>> {
        let mut output: Vec<T> = Vec::new();
        let brace = self.get_next_token()?;
        let is_virtual = match brace.value {
            TokenValue::LeftBrace => false,
            TokenValue::VirtualLeftBrace => true,
            _ => return Err(self.error("Expected left brace after let")),
        };
        if !((!is_virtual && self.is_next(TokenValue::RightBrace)?)
            || (is_virtual && self.is_next(TokenValue::VirtualRightBrace)?))
        {
            loop {
                let res = inner_parser(self, is_virtual)?;
                output.push(res);
                let tok = self.get_next_token()?;
                match tok.value {
                    TokenValue::Semicolon => {}
                    TokenValue::RightBrace if !is_virtual => break,
                    TokenValue::VirtualRightBrace if is_virtual => break,
                    _ => return Err(self.error("Unexpected token in braced list")),
                }
            }
        }
        Ok(output)
    }

    pub(super) fn parse_paren_list<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> error::Result<T>,
    ) -> error::Result<Vec<T>> {
        let mut output = Vec::new();
        self.expect(TokenValue::LeftParen)?;
        loop {
            if self.is_next(TokenValue::RightParen)? {
                break;
            }
            let res = inner_parser(self)?;
            output.push(res);
            let tok = self.get_next_token()?;
            match tok.value {
                TokenValue::Comma => {}
                TokenValue::RightParen => break,
                _ => return Err(self.error("Expected ',' or ')' in export list")),
            }
        }
        Ok(output)
    }

    pub(super) fn parse_var(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::LeftParen)? {
            let sym = self.parse_varsym()?;
            self.expect(TokenValue::RightParen)?;
            Ok(sym)
        } else {
            self.parse_varid()
        }
    }

    pub(super) fn parse_qvar(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::LeftParen)? {
            let sym = self.parse_qvarsym()?;
            self.expect(TokenValue::RightParen)?;
            Ok(sym)
        } else {
            self.parse_qvarid()
        }
    }

    pub(super) fn parse_con(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::LeftParen)? {
            let sym = self.parse_consym()?;
            self.expect(TokenValue::RightParen)?;
            Ok(sym)
        } else {
            self.parse_conid()
        }
    }

    pub(super) fn parse_qcon(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::LeftParen)? {
            let sym = self.parse_gconsym()?;
            self.expect(TokenValue::RightParen)?;
            Ok(sym)
        } else {
            self.parse_qconid()
        }
    }

    pub(super) fn parse_cname(&mut self) -> error::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_var)? {
            Ok(res)
        } else {
            self.parse_con()
                .map_err(|_| self.error("expected variable or constructor"))
        }
    }

    pub(super) fn parse_varop(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::Backtick)? {
            let sym = self.parse_varid()?;
            self.expect(TokenValue::Backtick)?;
            Ok(sym)
        } else {
            self.parse_varsym()
        }
    }

    pub(super) fn parse_qvarop(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::Backtick)? {
            let sym = self.parse_qvarid()?;
            self.expect(TokenValue::Backtick)?;
            Ok(sym)
        } else {
            self.parse_qvarsym()
        }
    }

    pub(super) fn parse_conop(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::Backtick)? {
            let sym = self.parse_conid()?;
            self.expect(TokenValue::Backtick)?;
            Ok(sym)
        } else {
            self.parse_consym()
        }
    }

    pub(super) fn parse_qconop(&mut self) -> error::Result<Identifier> {
        if self.is_next(TokenValue::Backtick)? {
            let sym = self.parse_qconid()?;
            self.expect(TokenValue::Backtick)?;
            Ok(sym)
        } else {
            self.parse_gconsym()
        }
    }

    pub(super) fn parse_op(&mut self) -> error::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_varop)? {
            Ok(res)
        } else {
            self.parse_conop()
                .map_err(|_| self.error("expected an operator"))
        }
    }

    pub(super) fn parse_qop(&mut self) -> error::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qvarop)? {
            Ok(res)
        } else {
            self.parse_qconop()
                .map_err(|_| self.error("expected an operator"))
        }
    }

    pub(super) fn parse_gcon(&mut self) -> error::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_unit)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_empty_list)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_tuplecon)? {
            Ok(res)
        } else {
            self.parse_qcon()
        }
    }
}
