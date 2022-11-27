use crate::ast::*;

use super::{
    lexing::{Token, TokenValue},
    ParserState,
};

impl<'a> ParserState<'a> {
    pub(super) fn parse_pattern(&mut self) -> anyhow::Result<Pattern> {
        let lpat = self.parse_lpattern()?;
        if let Some(op) = self.try_parse(&mut Self::parse_qconop)? {
            let rpat = self.parse_pattern()?;
            Ok(self.pattern(crate::ast::PatternValue::InfixConstructor(op, lpat, rpat)))
        } else {
            Ok(lpat)
        }
    }

    fn parse_lpattern(&mut self) -> anyhow::Result<Pattern> {
        if self.is_next(Token::varsym("-").value)? {
            match self.get_next_token()?.value {
                TokenValue::Float(f) => Ok(self.pattern(PatternValue::Literal(Const::Float(f)))),
                TokenValue::Integer(bn) => {
                    Ok(self.pattern(PatternValue::Literal(Const::Integer(bn))))
                }
                _ => Err(self.error("Minus in a pattern must be followed by number literal")),
            }
        } else if let Some(pat) = self.try_parse(&mut Self::parse_apattern)? {
            Ok(pat)
        } else {
            let gcon = self.parse_gcon()?;
            let pats = self.parse_some1(&mut Self::parse_apattern)?;
            Ok(self.pattern(PatternValue::Constructor(gcon, pats)))
        }
    }

    pub(super) fn parse_apattern(&mut self) -> anyhow::Result<Pattern> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::Tilde => {
                let pat = self.parse_apattern()?;
                Ok(self.pattern(PatternValue::Irrefutable(pat)))
            }
            TokenValue::LeftParen => {
                let mut pats =
                    self.parse_separated_by(&mut Self::parse_pattern, TokenValue::Comma)?;
                self.expect(TokenValue::RightParen)?;
                match pats.len() {
                    0 => Ok(self.pattern(PatternValue::Constructor(conid("()"), Vec::new()))),
                    1 => Ok(self.pattern(PatternValue::Wrapped(pats.pop().unwrap()))),
                    _ => Ok(self.pattern(PatternValue::Tuple(pats))),
                }
            }
            TokenValue::Underscore => Ok(self.pattern(PatternValue::Wildcard)),
            TokenValue::LeftBracket => {
                let pats = self.parse_separated_by(&mut Self::parse_pattern, TokenValue::Comma)?;
                self.expect(TokenValue::RightBracket)?;
                Ok(self.pattern(PatternValue::List(pats)))
            }
            TokenValue::Char(ch) => Ok(self.pattern(PatternValue::Literal(Const::Char(ch)))),
            TokenValue::Float(f) => Ok(self.pattern(PatternValue::Literal(Const::Float(f)))),
            TokenValue::Integer(bn) => Ok(self.pattern(PatternValue::Literal(Const::Integer(bn)))),
            TokenValue::String(s) => Ok(self.pattern(PatternValue::Literal(Const::String(s)))),
            _ => {
                self.rewind_lexer(1);
                if let Some(v) = self.try_parse(&mut Self::parse_var)? {
                    if self.is_next(TokenValue::At)? {
                        let aspat = self.parse_apattern()?;
                        Ok(self.pattern(PatternValue::As(v, aspat)))
                    } else {
                        Ok(self.pattern(PatternValue::Var(v)))
                    }
                } else if let Some(v) = self.try_parse(&mut |this| {
                    let con = this.parse_qcon()?;
                    this.expect(TokenValue::LeftBrace)?;
                    Ok(con)
                })? {
                    let pat_fields =
                        self.parse_separated_by(&mut Self::parse_pattern_field, TokenValue::Comma)?;
                    Ok(self.pattern(PatternValue::Labeled(v, pat_fields)))
                } else if let Some(con) = self.try_parse(&mut |this| {
                    let con = this.parse_gcon()?;
                    this.parse_none(&mut Self::parse_apattern)?;
                    Ok(con)
                })? {
                    Ok(self.pattern(PatternValue::Constructor(con, Vec::new())))
                } else {
                    Err(self.error("Invalid pattern"))
                }
            }
        }
    }

    fn parse_pattern_field(&mut self) -> anyhow::Result<(Identifier, Pattern)> {
        let var = self.parse_qvar()?;
        self.expect(TokenValue::Equals)?;
        let pat = self.parse_pattern()?;
        Ok((var, pat))
    }
}
