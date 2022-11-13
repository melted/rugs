use super::declaration::DeclKind;
use super::helpers::error;
use super::{ParserState};
use super::lexing::{Token, TokenValue};
use crate::{ast::*, location::Location};


impl<'a> ParserState<'a> {
    pub (super) fn parse_expression(&mut self) -> anyhow::Result<Expression> {
        let exp = self.parse_infix_expression()?;
        if self.is_next(TokenValue::DoubleColon)? {
            let context = self.try_parse(&mut |this| this.parse_context(false))?.unwrap_or(self.new_context());
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
            _ => self.parse_fexp()
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
        let decls = self.parse_braced_list(
            &mut |this, is_virtual| {
                let res = this.parse_declaration(DeclKind::Normal)?;
                if is_virtual && this.is_next(TokenValue::In)? {
                    this.push_token(TokenValue::VirtualRightBrace.into());
                }
                Ok(res)
            })?;
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
        unimplemented!()
    }

    fn parse_bracket_exp(&mut self) -> anyhow::Result<Expression> {
        unimplemented!()
    }

    fn parse_case_alt(&mut self, is_virtual : bool) -> anyhow::Result<CaseAlt> {
        unimplemented!()
    }

}