use super::{lexing::TokenValue, ParserState};
use crate::{ast::*, support::error};

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Instance,
    Class,
    Normal,
}

impl<'a> ParserState<'a> {
    pub(super) fn parse_top_declarations(&mut self) -> error::Result<Vec<TopDeclaration>> {
        let mut decls = Vec::new();
        let tok = self.peek_next_token()?;
        match tok.value {
            TokenValue::RightBrace | TokenValue::VirtualRightBrace => return Ok(decls),
            _t => {}
        }
        loop {
            let decl = self.parse_top_declaration()?;
            decls.push(decl);
            let tok = self.peek_next_token()?;
            match tok.value {
                TokenValue::Semicolon => {
                    self.get_next_token()?;
                }
                TokenValue::RightBrace | TokenValue::VirtualRightBrace => break,
                t => {
                    return Err(
                        self.error(&format!("expected semicolon or right brace, got {:?}", t))
                    )
                }
            }
        }
        Ok(decls)
    }

    pub(super) fn parse_top_declaration(&mut self) -> error::Result<TopDeclaration> {
        let tok = self.peek_next_token()?;
        match tok.value {
            TokenValue::Type => Ok(TopDeclaration::Type(self.parse_type_decl()?)),
            TokenValue::Data => Ok(TopDeclaration::Data(self.parse_data()?)),
            TokenValue::Newtype => Ok(TopDeclaration::Newtype(self.parse_newtype()?)),
            TokenValue::Class => Ok(TopDeclaration::Class(self.parse_class()?)),
            TokenValue::Instance => Ok(TopDeclaration::Instance(self.parse_instance()?)),
            TokenValue::Default => Ok(TopDeclaration::Default(self.parse_default_declaration()?)),
            TokenValue::Foreign => Ok(TopDeclaration::Foreign(self.parse_foreign_declaration()?)),
            _ => Ok(TopDeclaration::Declaration(
                self.parse_declaration(DeclKind::Normal)?,
            )),
        }
    }

    pub(super) fn parse_declaration(&mut self, decl_kind: DeclKind) -> error::Result<Declaration> {
        match &self.peek_next_token()?.value {
            TokenValue::Semicolon | TokenValue::VirtualRightBrace | TokenValue::RightBrace => {
                return Ok(self.new_declaration(DeclarationValue::Empty))
            }
            _ => (),
        };
        if decl_kind != DeclKind::Instance {
            if let Some(vars) = self.try_parse(&mut |this| {
                let vars = this.parse_separated_by(&mut Self::parse_var, TokenValue::Comma)?;
                this.expect(TokenValue::DoubleColon)?;
                Ok(vars)
            })? {
                let context = self.try_parse(&mut |this| this.parse_context(false))?;
                let ty = self.parse_type()?;
                return Ok(self.new_declaration(DeclarationValue::TypeSignature(vars, context, ty)));
            }
            if matches!(
                self.peek_next_token()?.value,
                TokenValue::Infix | TokenValue::Infixl | TokenValue::Infixr
            ) {
                return self.parse_fixity_declaration();
            }
        }
        if let Some(fun) = self.try_parse(&mut Self::parse_function_lhs)? {
            let bind = self.parse_function_rhs()?;
            Ok(self.new_declaration(DeclarationValue::FunBind(fun, bind)))
        } else if decl_kind == DeclKind::Normal {
            let pat = self.parse_pattern()?;
            let bind = self.parse_function_rhs()?;
            Ok(self.new_declaration(DeclarationValue::PatBind(pat, bind)))
        } else {
            let var = self.parse_var()?;
            let bind = self.parse_function_rhs()?;
            Ok(self.new_declaration(DeclarationValue::VarBind(var, bind)))
        }
    }

    pub(super) fn parse_function_lhs(&mut self) -> error::Result<FunBind> {
        if let Some(fun) = self.try_parse(&mut Self::parse_function_prefix)? {
            Ok(fun)
        } else if let Some(fun) = self.try_parse(&mut Self::parse_function_infix)? {
            Ok(fun)
        } else if let Some(fun) = self.try_parse(&mut Self::parse_function_wrapped)? {
            Ok(fun)
        } else {
            Err(self.error("Bad function definition lhs"))
        }
    }

    pub(super) fn parse_function_prefix(&mut self) -> error::Result<FunBind> {
        let var = self.parse_var()?;
        let pats = self.parse_some1(&mut Self::parse_apattern)?;
        Ok(FunBind::Plain(var, pats))
    }

    pub(super) fn parse_function_infix(&mut self) -> error::Result<FunBind> {
        let lhs = self.parse_pattern()?;
        let op = self.parse_varop()?;
        let rhs = self.parse_pattern()?;
        Ok(FunBind::Op(op, lhs, rhs))
    }

    pub(super) fn parse_function_wrapped(&mut self) -> error::Result<FunBind> {
        self.expect(TokenValue::LeftParen)?;
        let fun = self.parse_function_lhs()?;
        self.expect(TokenValue::RightParen)?;
        let pats = self.parse_some1(&mut Self::parse_apattern)?;
        Ok(FunBind::Wrapped(Box::new(fun), pats))
    }

    pub(super) fn parse_function_rhs(&mut self) -> error::Result<Binding> {
        if self.is_next(TokenValue::Equals)? {
            let exp = self.parse_expression()?;
            let where_decls = self.parse_optional_where()?;
            Ok(Binding::Plain(exp, where_decls))
        } else {
            let rhs = self.parse_some1(&mut Self::parse_function_rhs_guarded)?;
            let where_decls = self.parse_optional_where()?;
            Ok(Binding::Guarded(rhs, where_decls))
        }
    }

    pub(super) fn parse_optional_where(&mut self) -> error::Result<Vec<Declaration>> {
        if self.is_next(TokenValue::Where)? {
            self.parse_some1(&mut |this| this.parse_declaration(DeclKind::Normal))
        } else {
            Ok(Vec::new())
        }
    }

    pub(super) fn parse_function_rhs_guarded(&mut self) -> error::Result<GuardedExpression> {
        let guards = self.parse_guards()?;
        self.expect(TokenValue::Equals)?;
        let exp = self.parse_expression()?;
        Ok(GuardedExpression { guards, body: exp })
    }

    pub(super) fn parse_guards(&mut self) -> error::Result<Vec<SeqSyntax>> {
        self.expect(TokenValue::Bar)?;
        self.parse_separated_by(
            &mut |this| this.parse_seqsyntax(SeqKind::Guard),
            TokenValue::Comma,
        )
    }

    pub(super) fn parse_default_declaration(&mut self) -> error::Result<Vec<Type>> {
        self.expect(TokenValue::Default)?;
        self.parse_paren_list(&mut Self::parse_type)
    }

    pub(super) fn parse_fixity_declaration(&mut self) -> error::Result<Declaration> {
        let assoc = match self.get_next_token()?.value {
            TokenValue::Infix => Association::NonAssociative,
            TokenValue::Infixl => Association::Left,
            TokenValue::Infixr => Association::Right,
            _ => return Err(self.error("Expected infix declaration")),
        };
        let prec = match self.peek_next_token()?.value {
            TokenValue::Integer(bn) => {
                let n: u32 = bn.try_into().unwrap_or(1000);
                if n > 9 {
                    return Err(self.error("precedence must be betweem 0 and 9"));
                }
                n
            }
            _ => 9,
        };
        let ops = self.parse_separated_by(&mut Self::parse_op, TokenValue::Comma)?;
        Ok(self.new_declaration(DeclarationValue::Fixity(ops, assoc, prec)))
    }
}
