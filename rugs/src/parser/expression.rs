use super::helpers::error;
use super::{ParserState};
use super::lexing::{Token, TokenValue};
use crate::{ast::*, location::Location};


impl<'a> ParserState<'a> {
    pub (super) fn parse_expression(&mut self) -> anyhow::Result<Expression> {
        let exp = self.parse_infix_expression()?;
        if self.is_next(TokenValue::DoubleColon)? {
            // TODO: Parse context and type
            Ok(self.typed(exp, Vec::new(), Type::Base(conid("A"))))
        } else {
            Ok(exp)
        }
    }

    fn parse_infix_expression(&mut self) -> anyhow::Result<Expression> {
        if self.is_next(Token::varsym("-").value)? {
            let exp = self.parse_expression()?;
            let negate = self.var(varid("negate"));
            return Ok(self.app(negate, exp));
        }
        let exp = self.parse_lexp()?;
        if let Some(op) = self.maybe_qop()? {
            let exp_right = self.parse_infix_expression()?;
            Ok(self.infix(op, exp, exp_right))
        } else {
            Ok(exp)
        }
    }

    fn maybe_qop(&mut self) -> anyhow::Result<Option<Identifier>> {
        let op = match self.peek_next_token()?.value {
            TokenValue::Colon => consym(":") ,
            TokenValue::QConSym(module, con) => qconsym(&module, &con),
            TokenValue::QVarSym(module, op) => qvarsym(&module, &op),
            TokenValue::ConSym(con) => consym(&con),
            TokenValue::VarSym(op) => varsym(&op),
            TokenValue::Backtick => {
                let backtick = self.get_next_token()?;
                let op = match self.get_next_token()?.value {
                    TokenValue::QVarId(module, var) =>
                        qvarid(&module, &var),
                    TokenValue::QConId(module, con) =>
                        qconid(&module, &con),
                    TokenValue::VarId(var) => varid(&var),
                    TokenValue::ConId(con) => conid(&con),
                    _ => return error("Expected identifier after backtick", backtick.location).into()
                };
                self.expect(TokenValue::Backtick)?;
                return Ok(Some(op));
            },
            _ => return Ok(None)
        };
        self.get_next_token()?;
        Ok(Some(op))
    }

    fn parse_lexp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::Backslash => {
                let mut args = Vec::new();
                loop {
                    let pat = self.parse_apattern()?;
                    args.push(pat);
                    if self.is_next(TokenValue::RightArrow)? {
                        break;
                    }
                }
                let exp = self.parse_expression()?;
                Ok(self.lambda(args, exp))
            },
            TokenValue::Let => {
                let decls = self.parse_braced_list(
                    |this, is_virtual| {
                        let res = this.parse_declaration()?;
                        if is_virtual && this.is_next(TokenValue::In)? {
                            this.push_token(TokenValue::VirtualRightBrace.into());
                        }
                        Ok(res)
                    })?;
                let exp = self.parse_expression()?;
                Ok(self.let_expression(decls, exp))
            },
            TokenValue::If => {
                let predicate = self.parse_expression()?;
                self.optional_semicolon()?;
                self.expect(TokenValue::Then)?;
                let then_exp = self.parse_expression()?;
                self.optional_semicolon()?;
                self.expect(TokenValue::Else)?;
                let else_exp = self.parse_expression()?;
                Ok(self.if_expression(predicate, then_exp, else_exp))
            },
            TokenValue::Case => {
                let exp = self.parse_expression()?;
                self.expect(TokenValue::Of)?;
                let alts = self.parse_braced_list(ParserState::parse_case_alt)?;
                Ok(self.case_expression(exp, alts))
            },
            TokenValue::Do => {
                unimplemented!()
            },
            _ => {
                self.push_token(tok);
                self.parse_fexp()
            }
        }
    }

    fn parse_fexp(&mut self) -> anyhow::Result<Expression> {
        let mut aexps = Vec::new();
        while self.is_aexp()? {
            let exp = self.parse_aexp()?;
            aexps.push(exp);
        }
        let mut exp = match aexps.pop() {
            Some(e) => e,
            None => {
                return error("Expected aexp", Location::Offset { start: self.pos, end: self.pos });
            }
        };
        while let Some(f) = aexps.pop() {
            exp = self.app(f, exp);
        }
        Ok(exp)
    }

    fn is_aexp(&mut self) -> anyhow::Result<bool> {
        let tok = self.peek_next_token()?;
        match tok.value {
            TokenValue::LeftParen | TokenValue::LeftBracket | TokenValue::ConId(_)
            | TokenValue::QConId(_, _) | TokenValue::QVarId(_, _) | TokenValue::VarId(_)
            | TokenValue::Char(_) | TokenValue::Integer(_) | TokenValue::Float(_)
            | TokenValue::String(_) => Ok(true),
            _ => Ok(false)
        }
    }

    fn parse_aexp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.get_next_token()?;
        let expr = match tok.value {
            TokenValue::LeftParen => self.parse_parens_exp()?,
            TokenValue::LeftBracket => self.parse_bracket_exp()?,
            TokenValue::QConId(_, _) | TokenValue::QVarId(_, _) | TokenValue::ConId(_) | TokenValue::VarId(_) => self.var(Identifier::try_from(tok)?),
            TokenValue::Char(ch) => self.char_const(&ch),
            TokenValue::Float(d) => self.float(&d),
            TokenValue::Integer(bn) => self.integer(bn),
            TokenValue::String(s) => self.string_const(&s),
            t => return error(&format!("unexpected token {:?} in aexp", t), tok.location)
        };
        if self.is_next(TokenValue::LeftBrace)? {
            // Record stuff
            unimplemented!()
        } else {
            Ok(expr)
        }
    }

    fn parse_parens_exp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.peek_next_token()?;
        let exp = match tok.value {
            TokenValue::RightParen => {
                self.get_next_token()?;
                return Ok(self.var(conid("()")))
            },
            TokenValue::Comma => {
                let mut con = "(".to_string();
                while self.is_next(TokenValue::Comma)? {
                    con.push(',');
                }
                self.expect(TokenValue::RightParen)?;
                con.push(')');
                return Ok(self.var(conid(&con)))
            },
            _ => self.parse_expression()?
        };
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::RightParen => Ok(self.wrapped(exp)),
            TokenValue::Comma => {
                let mut tuple = vec![exp];
                loop {
                    let e = self.parse_expression()?;
                    tuple.push(e);
                    let tok = self.get_next_token()?;
                    match tok.value {
                        TokenValue::Comma => {},
                        TokenValue::RightParen => break,
                        _ => return error(&format!("Unexpected token {:?} in tuple expression", tok), tok.location)
                    }
                }
                Ok(self.tuple(tuple))
            }
            _ => return error(&format!("Expected right paren or comma, got {:?}", tok), tok.location)
        }
    }

    fn parse_bracket_exp(&mut self) -> anyhow::Result<Expression> {
        unimplemented!()
    }

    fn parse_case_alt(&mut self, is_virtual : bool) -> anyhow::Result<CaseAlt> {
        unimplemented!()
    }
}