use crate::{ast::*, error};

use super::{ParserState, lexing::{TokenValue, Token}, declaration::DeclKind};

impl<'a> ParserState<'a> {
    pub (super) fn parse_class(&mut self) -> anyhow::Result<Class> {
        self.expect(TokenValue::Class)?;
        let context = self.try_parse_scontext()?;
        let mut tok = Token::default_conid();
        self.expect_token_value(&mut tok)?;
        let name = Identifier::try_from(tok)?;
        tok = Token::default_varid();
        self.expect_token_value(&mut tok)?;
        let var = Identifier::try_from(tok)?;
        let mut class = self.new_class(name);
        if self.is_next(TokenValue::Where)? {
            let decls = self.parse_braced_list(
                |this, _| {
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
        unimplemented!()
    }

    pub (super) fn parse_newtype(&mut self) -> anyhow::Result<Newtype> {
        unimplemented!()
    }

    
    pub (super) fn parse_type_decl(&mut self) -> anyhow::Result<TypeDecl> {
        unimplemented!()
    }

    pub (super) fn parse_type(&mut self) -> anyhow::Result<Type> {
        unimplemented!()
    }

    pub (super) fn parse_simpletype(&mut self) -> anyhow::Result<Type> {
        unimplemented!()
    }

    pub (super) fn parse_instance(&mut self) -> anyhow::Result<Instance> {
        unimplemented!()
    }

    pub (super) fn parse_foreign_declaration(&mut self) -> anyhow::Result<Foreign> {
        unimplemented!()
    }

    pub (super) fn try_parse_context(&mut self) -> anyhow::Result<Vec<Context>> {
        unimplemented!()
    }

    pub (super) fn try_parse_scontext(&mut self) -> anyhow::Result<Vec<Context>> {
        unimplemented!()
    }
}