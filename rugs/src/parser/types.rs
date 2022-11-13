use crate::{ast::*};

use super::{ParserState, lexing::{TokenValue, Token}, declaration::DeclKind};

impl<'a> ParserState<'a> {
    pub (super) fn parse_class(&mut self) -> anyhow::Result<Class> {
        self.expect(TokenValue::Class)?;
        let context = self.try_parse(&mut Self::parse_scontext)?
                                    .unwrap_or_else(|| self.new_context());
        let name = self.parse_conid()?;
        let var = self.parse_varid()?;
        let mut class = self.new_class(name);
        if self.is_next(TokenValue::Where)? {
            let decls = self.parse_braced_list(
                &mut |this, _| {
                    this.parse_declaration(DeclKind::Class)
                }
            )?;
            class.decls = decls;
        }
        class.context = context;
        class.tyvars.push(var);
        Ok(class)
    }

    pub (super) fn parse_data(&mut self) -> anyhow::Result<Data> {
        self.expect(TokenValue::Data)?;
        let the_type = self.parse_simpletype()?;
        let mut data = self.new_data(the_type);
        if self.is_next(TokenValue::Equals)? {
            data.constructors = self.parse_separated_by(&mut Self::parse_constructor, TokenValue::Bar)?;
        }
        data.deriving = self.try_parse(&mut Self::parse_deriving)?.unwrap_or_default();
        Ok(data)
    }

    pub (super) fn parse_newtype(&mut self) -> anyhow::Result<Newtype> {
        self.expect(TokenValue::Newtype)?;
        let the_type = self.parse_simpletype()?;
        self.expect(TokenValue::Equals)?;
        let constructor = self.parse_newtype_constructor()?;
        let mut newtype = self.new_newtype(the_type, constructor);
        newtype.deriving = self.try_parse(&mut Self::parse_deriving)?.unwrap_or_default();
        Ok(newtype)
    }

    
    pub (super) fn parse_type_decl(&mut self) -> anyhow::Result<TypeDecl> {
        self.expect(TokenValue::Type)?;
        let this_type = self.parse_simpletype()?;
        self.expect(TokenValue::Equals)?;
        let that_type = self.parse_type()?;
        Ok(self.new_typedecl(this_type, that_type)  )
    }

    pub (super) fn parse_type(&mut self) -> anyhow::Result<Type> {
        let mut the_type = self.parse_btype()?;
        if self.is_next(TokenValue::RightArrow)? {
            let res = self.parse_type()?;
            the_type = the_type.fun(res);
        }
        Ok(the_type)
    }

    pub (super) fn parse_btype(&mut self) -> anyhow::Result<Type> {
        let mut the_type = self.parse_atype()?;
        let arg_types = self.parse_some(&mut Self::parse_atype)?;
        for t in arg_types {
            the_type = the_type.app(t);
        }
        Ok(the_type)
    }

    pub (super) fn parse_atype(&mut self) -> anyhow::Result<Type> {
        if let Some(res) = self.try_parse(&mut Self::parse_gtycon)? {
            Ok(Type::base(res))
        } else if let Some(res) = self.try_parse(&mut Self::parse_varid)? {
            Ok(Type::var(res))
        } else if let Some(res) = self.try_parse(&mut |this|this.parse_paren_list(&mut Self::parse_type))? {
            Ok(Type::tuple(res))
        } else if let Some(res) = self.try_parse(&mut |this| this.parse_surrounded_by(TokenValue::LeftBracket, &mut Self::parse_type, TokenValue::RightBracket))? {
            Ok(Type::list(res))
        } else if let Some(res) = self.try_parse(&mut |this| this.parse_surrounded_parens(&mut Self::parse_type))? {
            Ok(res)
        } else {
            Err(self.error("Not an atype"))
        }
    }

    pub (super) fn parse_simpletype(&mut self) -> anyhow::Result<Type> {
        let tycon = self.parse_conid()?;
        let tyvars = self.parse_some(&mut Self::parse_varid)?;
        let mut the_type = Type::base(tycon);
        for v in tyvars {
            the_type = the_type.app(Type::var(v));
        }
        Ok(the_type)
    }

    pub (super) fn parse_gtycon(&mut self) -> anyhow::Result<Identifier> {
        if let Some(res) = self.try_parse(&mut Self::parse_qcon)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_unit)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_empty_list)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_funcon)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_tuplecon)? {
            Ok(res)
        } else {
            Err(self.error("Not a gcon"))
        }
    }

    pub (super) fn parse_constructor(&mut self) -> anyhow::Result<Constructor> {
        if let Some(res) = self.try_parse(&mut Self::parse_prefix_constructor)? {
            Ok(res)
        } else if let Some(res) = self.try_parse(&mut Self::parse_infix_constructor)? {
            Ok(res)
        } else {
            Err(self.error("expected constructor"))
        }
    }

    pub (super) fn parse_infix_constructor(&mut self) -> anyhow::Result<Constructor> {
        fn get_type(this : &mut ParserState) -> anyhow::Result<(bool, Type)> {
            if this.is_next(Token::varsym("!").value)? {
                this.parse_atype().map(|t| (true, t))
            } else {
                this.parse_btype().map(|t| (false, t))
            }
        }
        let lhs = get_type(self)?;
        let op = self.parse_conop()?;
        let rhs = get_type(self)?;
        Ok(Constructor::Plain { con: op, the_types: vec![lhs, rhs] })
    }

    pub (super) fn parse_prefix_constructor(&mut self) -> anyhow::Result<Constructor> {
        let con = self.parse_con()?;
        if self.is_next(TokenValue::LeftBrace)? {
            let raw = self.parse_separated_by(&mut Self::parse_field_declaration, TokenValue::Comma)?;
            self.expect(TokenValue::RightBrace)?;
            let mut fields = Vec::new();
            for v in &raw {
                fields.extend_from_slice(v);
            }
            Ok(Constructor::Labelled { con: con, fields: fields })
        } else {
            let args = self.parse_some(&mut |this| {
                let bang = this.is_next(Token::varsym("!").value)?;
                let t = this.parse_atype()?;
                Ok((bang, t))
            })?;
            Ok(Constructor::Plain { con: con, the_types: args })
        }
    }

    pub (super) fn parse_field_declaration(&mut self) -> anyhow::Result<Vec<TypeField>> {
        let vars = self.parse_separated_by(&mut Self::parse_var, TokenValue::Comma)?;
        self.expect(TokenValue::DoubleColon)?;
        let is_strict = self.is_next(Token::varsym("!").value)?;
        let t = if is_strict {
            self.parse_atype()?
        } else {
            self.parse_type()?
        };
        let mut output = Vec::new();
        for v in vars {
            output.push(TypeField { label: v, the_type: t.clone(), is_strict: is_strict})
        }
        Ok(output)
    }

    pub (super) fn parse_newtype_constructor(&mut self) -> anyhow::Result<Constructor> {
        let con = self.parse_con()?;
        if self.is_next(TokenValue::LeftBrace)? {
            let var = self.parse_var()?;
            self.expect(TokenValue::DoubleColon)?;
            let t = self.parse_type()?;
            self.expect(TokenValue::RightBrace)?;
            let v = vec![TypeField { label: var, the_type: t, is_strict: true}];
            Ok(Constructor::Labelled { con: con, fields: v })
        } else {
            let t = self.parse_atype()?;
            Ok(Constructor::Plain { con: con, the_types: vec![(true, t)] })
        }
    }

    pub (super) fn parse_deriving(&mut self) -> anyhow::Result<Vec<Identifier>> {
        if self.is_next(TokenValue::LeftParen)? {
            let classes = self.parse_separated_by(&mut Self::parse_qconid, TokenValue::Comma)?;
            self.expect(TokenValue::RightParen)?;
            Ok(classes)
        } else {
            let tycls = self.parse_qconid()?;
            Ok(vec![tycls])
        }
    }

    pub (super) fn parse_instance(&mut self) -> anyhow::Result<Instance> {
        self.expect(TokenValue::Instance)?;
        let context = self.try_parse(&mut Self::parse_scontext)?
                                    .unwrap_or_else(|| self.new_context());
        let qtycls = self.parse_qconid()?;
        let t = if self.is_next(TokenValue::LeftParen)? {
            if let Some(tcon) = self.try_parse(&mut Self::parse_gtycon)? {
                let vars = self.parse_some(&mut Self::parse_varid)?;
                Type::Simple { base: tcon, tyvars: vars }
            } else {
                let var = self.parse_varid()?;
                if self.is_next(TokenValue::RightArrow)? {
                    let var2 = self.parse_varid()?;
                    if var == var2 {
                        return Err(self.error("type variables must be distinct"));
                    }
                    Type::fun(Type::var(var), Type::var(var2))
                } else {
                    self.expect(TokenValue::Comma)?;
                    let mut vars = self.parse_separated_by(&mut Self::parse_varid, TokenValue::Comma)?;
                    vars.insert(0, var);
                    let mut sorted = vars.clone();
                    sorted.sort();
                    sorted.dedup();
                    if sorted.len() < vars.len() {
                        return Err(self.error("type variables must be distinct"));
                    }
                    let tvars = vars.into_iter().map(|v| Type::var(v)).collect();
                    Type::tuple(tvars)
                }
            }
        } else if self.is_next(TokenValue::LeftBracket)? {
            let var = self.parse_varid()?;
            self.expect(TokenValue::RightBracket)?;
            Type::list(Type::Var(var))
        } else {
            Type::Base(self.parse_gtycon()?)
        };
        let mut instance = self.new_instance(qtycls, t);
        instance.context = context;
        if self.is_next(TokenValue::Where)? {
            let decls = self.parse_braced_list(
                &mut |this, _| {
                    this.parse_declaration(DeclKind::Instance)
                }
            )?;
            instance.decls = decls;
        }
        Ok(instance)
    }

    pub (super) fn parse_foreign_declaration(&mut self) -> anyhow::Result<Foreign> {
        self.expect(TokenValue::Foreign)?;
        if self.is_next(Token::varid("import").value)? {
            let callconv = self.parse_word()?;
            let safety = self.parse_safety()?;
            let impent = match self.peek_next_token()?.value {
                TokenValue::String(s) => Some(s),
                _ => None
            };
            let var = self.parse_var()?;
            let ftype = self.parse_ftype()?;
            Ok(self.new_foreign(ForeignDeclaration::Import { callconv: callconv, impent: impent, safety: safety, var: var, ftype: ftype }))
        } else if self.is_next(Token::varid("export").value)? {
            let callconv = self.parse_word()?;
            let expent = match self.peek_next_token()?.value {
                TokenValue::String(s) => Some(s),
                _ => None
            };
            let var = self.parse_var()?;
            let ftype = self.parse_ftype()?;
            Ok(self.new_foreign(ForeignDeclaration::Export { callconv: callconv, expent: expent, var: var, ftype: ftype }))
        } else {
            Err(self.error("`foreign` must be followed by `import` or `export`"))
        }
    }

    pub (super) fn parse_ftype(&mut self) -> anyhow::Result<Type> {
        if self.is_next(TokenValue::LeftParen)? {
            self.expect(TokenValue::RightParen)?;
            // TODO: Error if right arrow next
            return Ok(Type::base(conid("()")));
        }
        let tycon = self.parse_qconid()?;
        let args = self.parse_some(&mut Self::parse_atype)?;
        let mut ty = Type::base(tycon);
        for a in args {
            ty = ty.app(a);
        }
        if self.is_next(TokenValue::RightArrow)? {
            let rtype = self.parse_ftype()?;
            Ok(ty.fun(rtype))
        } else {
            Ok(ty)
        }
    }

    pub (super) fn parse_safety(&mut self) -> anyhow::Result<Option<Safety>> {
        if self.is_next(Token::varid("safe").value)? {
            Ok(Some(Safety::Safe))
        } else if self.is_next(Token::varid("unsafe").value)? {
            Ok(Some(Safety::Unsafe))
        } else {
            Ok(None)
        }
    }

    pub (super) fn parse_context(&mut self) -> anyhow::Result<Context> {
        unimplemented!()
    }

    pub (super) fn parse_context_class(&mut self) -> anyhow::Result<Context> {
        unimplemented!()
    }

    pub (super) fn parse_scontext(&mut self) -> anyhow::Result<Context> {
        unimplemented!()
    }
}