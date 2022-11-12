use crate::{ast::*, error};

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
        unimplemented!()
    }

    pub (super) fn parse_newtype_constructor(&mut self) -> anyhow::Result<Constructor> {
        unimplemented!()
    }

    pub (super) fn parse_deriving(&mut self) -> anyhow::Result<Vec<Identifier>> {
        unimplemented!()
    }

    pub (super) fn parse_instance(&mut self) -> anyhow::Result<Instance> {
        unimplemented!()
    }

    pub (super) fn parse_foreign_declaration(&mut self) -> anyhow::Result<Foreign> {
        unimplemented!()
    }

    pub (super) fn parse_context(&mut self) -> anyhow::Result<Context> {
        unimplemented!()
    }

    pub (super) fn parse_scontext(&mut self) -> anyhow::Result<Context> {
        unimplemented!()
    }
}