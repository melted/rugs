use super::declaration::DeclKind;
use super::helpers::error;
use super::lexing::{Token, TokenValue};
use super::ParserState;
use crate::{ast::*, location::Location};

impl<'a> ParserState<'a> {
    pub(super) fn parse_expression(&mut self) -> anyhow::Result<Expression> {
        let exp = self.parse_infix_expression()?;
        if self.is_next(TokenValue::DoubleColon)? {
            let context = self
                .try_parse(&mut |this| this.parse_context(false))?
                .unwrap_or(self.new_context());
            let ty = self.parse_type()?;
            Ok(self.typed(exp, context, ty))
        } else {
            Ok(exp)
        }
    }

    fn parse_infix_expression(&mut self) -> anyhow::Result<Expression> {
        if self.is_next(Token::varsym("-").value)? {
            let exp = self.parse_expression()?;
            let negate = self.var(varid("-"));
            return Ok(self.app(negate, exp)); // TODO: This will need to take part in precedence later
        }
        let exp = self.parse_lexp()?;
        if let Some(op) = self.try_parse(&mut Self::parse_qop)? {
            let exp_right = self.parse_infix_expression()?;
            Ok(self.infix(op, exp, exp_right))
        } else {
            Ok(exp)
        }
    }

    fn parse_lexp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.peek_next_token()?;
        match tok.value {
            TokenValue::Backslash => self.parse_lambda_expression(),
            TokenValue::Let => self.parse_let_expression(),
            TokenValue::If => self.parse_if_expression(),
            TokenValue::Case => self.parse_case_expression(),
            TokenValue::Do => self.parse_do_expression(),
            _ => self.parse_fexp(),
        }
    }

    fn parse_fexp(&mut self) -> anyhow::Result<Expression> {
        let mut exp = self.parse_aexp()?;
        let aexps = self.parse_some(&mut Self::parse_aexp)?;
        for arg in aexps {
            exp = self.app(exp, arg);
        }
        Ok(exp)
    }

    fn parse_aexp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.get_next_token()?;
        let tok2 = tok.clone();
        let expr = match tok.value {
            TokenValue::LeftParen => self.parse_parens_exp()?,
            TokenValue::LeftBracket => self.parse_bracket_exp()?,
            TokenValue::QConId(_, _)
            | TokenValue::QVarId(_, _)
            | TokenValue::ConId(_)
            | TokenValue::VarId(_) => self.var(Identifier::try_from(tok)?),
            TokenValue::Char(ch) => self.char_const(&ch),
            TokenValue::Float(d) => self.float(&d),
            TokenValue::Integer(bn) => self.integer(bn),
            TokenValue::String(s) => self.string_const(&s),
            t => return error(&format!("unexpected token {:?} in aexp", t), tok.location),
        };
        if self.peek_next(TokenValue::LeftBrace)? {
            let rec_expr = self.parse_record_expression()?;
            match tok2.value {
                TokenValue::QConId(_, _) | TokenValue::ConId(_) => Ok(self.record_constr(Token::try_into(tok2)?, rec_expr)),
                _ => {
                    if rec_expr.is_empty() {
                        Err(self.error("Empty record update"))
                    } else {
                        Ok(self.record_update(expr, rec_expr))
                    }
                }
            }

        } else {
            Ok(expr)
        }
    }

    fn parse_parens_exp(&mut self) -> anyhow::Result<Expression> {
        let tok = self.peek_next_token()?;
        let exp = match tok.value {
            TokenValue::RightParen => {
                self.get_next_token()?;
                return Ok(self.var(conid("()")));
            }
            TokenValue::Comma => {
                let mut con = "(".to_string();
                while self.is_next(TokenValue::Comma)? {
                    con.push(',');
                }
                self.expect(TokenValue::RightParen)?;
                con.push(')');
                return Ok(self.var(conid(&con)));
            }
            _ => self.parse_expression()?,
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
                        TokenValue::Comma => {}
                        TokenValue::RightParen => break,
                        _ => {
                            return error(
                                &format!("Unexpected token {:?} in tuple expression", tok),
                                tok.location,
                            )
                        }
                    }
                }
                Ok(self.tuple(tuple))
            }
            _ => {
                return error(
                    &format!("Expected right paren or comma, got {:?}", tok),
                    tok.location,
                )
            }
        }
    }

    fn parse_if_expression(&mut self) -> anyhow::Result<Expression> {
        self.expect(TokenValue::If)?;
        let predicate = self.parse_expression()?;
        self.optional_semicolon()?;
        self.expect(TokenValue::Then)?;
        let then_exp = self.parse_expression()?;
        self.optional_semicolon()?;
        self.expect(TokenValue::Else)?;
        let else_exp = self.parse_expression()?;
        Ok(self.if_expression(predicate, then_exp, else_exp))
    }

    fn parse_let_expression(&mut self) -> anyhow::Result<Expression> {
        self.expect(TokenValue::Let)?;
        let decls = self.parse_braced_list(&mut |this, is_virtual| {
            let res = this.parse_declaration(DeclKind::Normal)?;
            Ok(res)
        })?;
        self.expect(TokenValue::In)?;
        let exp = self.parse_expression()?;
        Ok(self.let_expression(decls, exp))
    }

    fn parse_lambda_expression(&mut self) -> anyhow::Result<Expression> {
        self.expect(TokenValue::Backslash)?;
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
    }

    fn parse_case_expression(&mut self) -> anyhow::Result<Expression> {
        self.expect(TokenValue::Case)?;
        let exp = self.parse_expression()?;
        self.expect(TokenValue::Of)?;
        let alts = self.parse_braced_list(&mut ParserState::parse_case_alt)?;
        Ok(self.case_expression(exp, alts))
    }

    fn parse_do_expression(&mut self) -> anyhow::Result<Expression> {
        self.expect(TokenValue::Do)?;
        let stmts = self.parse_braced_list(&mut |this, _| this.parse_seqsyntax(SeqKind::Do))?;
        Ok(self.do_expression(stmts))
    }

    fn parse_bracket_exp(&mut self) -> anyhow::Result<Expression> {
        if let Some((start, step)) = self.try_parse(&mut |this| {
            let first = this.parse_expression()?;
            let step = if this.is_next(TokenValue::Comma)? {
                Some(this.parse_expression()?)
            } else {
                None
            };
            this.expect(TokenValue::DotDot)?;
            Ok((first, step))
        })? {
            if self.is_next(TokenValue::RightBracket)? {
                match step {
                    Some (a) => {
                        let f = self.var(varid("enumFromThen"));
                        Ok(self.apps(f, vec![start, a]))
                    },
                    None => {
                        let f = self.var(varid("enumFrom"));
                        Ok(self.app(f, start))
                    }
                }
            } else {
                let stop = self.parse_expression()?;
                self.expect(TokenValue::RightBracket)?;
                match step {
                    Some (a) => {
                        let f = self.var(varid("enumFromThenTo"));
                        Ok(self.apps(f, vec![start, a, stop]))
                    },
                    None => {
                        let f = self.var(varid("enumFromTo"));
                        Ok(self.apps(f, vec![start, stop]))
                    },
                }
            }
        } else if let Some(exp) = self.try_parse(&mut |this| {
            let first = this.parse_expression()?;
            this.expect(TokenValue::Bar)?;
            Ok(first)
        })? {
            let quals = self.parse_separated_by(&mut |this| this.parse_seqsyntax(SeqKind::Comprehension), TokenValue::Comma)?;
            Ok(self.comprehension(exp, quals))
        } else {
            let exps = self.parse_separated_by(&mut Self::parse_expression, TokenValue::Comma)?;
            self.expect(TokenValue::RightBracket)?;
            Ok(self.list(exps))
        }
    }

    fn parse_case_alt(&mut self, is_virtual: bool) -> anyhow::Result<CaseAlt> {
        let pat = self.parse_pattern()?;
        if self.is_next(TokenValue::RightArrow)? {
            let exp = self.parse_expression()?;
            Ok(CaseAlt::Simple(pat, exp))
        } else {
            let guardexps = self.parse_some1(&mut Self::parse_case_guarded)?;
            Ok(CaseAlt::Guarded(pat, guardexps))
        }  
    }

    pub (super) fn parse_case_guarded(&mut self) -> anyhow::Result<GuardedExpression> {
        let guards = self.parse_guards()?;
        self.expect(TokenValue::RightArrow)?;
        let exp = self.parse_expression()?;
        Ok(GuardedExpression { guards: guards, body: exp })
    }

    pub (super) fn parse_seqsyntax(&mut self, kind: SeqKind) -> anyhow::Result<SeqSyntax> {
        let expfn = if kind == SeqKind::Guard {
            |this:&mut ParserState| this.parse_infix_expression()
        } else {
            |this:&mut ParserState| this.parse_expression()
        };
        if self.is_next(TokenValue::Let)? {
            let decls = self.parse_braced_list(&mut |this,_| this.parse_declaration(DeclKind::Normal))?;
            Ok(SeqSyntax::Decls(decls))
        } else if let Some(pat) = self.try_parse(&mut |this|{
            let pat = this.parse_pattern()?;
            this.expect(TokenValue::LeftArrow)?;
            Ok(pat)
        })? {
            let exp = expfn(self)?;
            Ok(SeqSyntax::Pattern(pat, exp))
        } else if kind == SeqKind::Do && self.is_next(TokenValue::Semicolon)? {
                Ok(SeqSyntax::Empty)
        } else {
            let exp = expfn(self)?;
            Ok(SeqSyntax::Expr(exp))
        }
    }

    pub (super) fn parse_record_expression(&mut self) -> anyhow::Result< Vec<(Identifier, Expression)>> {
        self.parse_separated_by(&mut Self::parse_record_field, TokenValue::Comma)
    }

    pub (super) fn parse_record_field(&mut self) -> anyhow::Result<(Identifier, Expression)> {
        let var = self.parse_qvar()?;
        self.expect(TokenValue::Equals)?;
        let exp = self.parse_expression()?;
        Ok((var, exp))
    }
}
