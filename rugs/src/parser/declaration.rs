

use crate::{ast::*};
use super::{ParserState, lexing::{TokenValue, Token}, helpers::error};

pub enum DeclKind{
    Instance,
    Class,
    Normal
}

impl<'a> ParserState<'a> {
    pub (super) fn parse_top_declarations(&mut self) -> anyhow::Result<Vec<TopDeclaration>> {
        let mut decls = Vec::new();
        loop {
            decls.push(self.parse_top_declaration()?);
            let tok = self.peek_next_token()?;
            match tok.value {
                TokenValue::Semicolon => {
                    self.get_next_token()?;
                },
                TokenValue::RightBrace | TokenValue::VirtualRightBrace => break,
                _ => return error("expected semicolon or right brace", tok.location)
            }
        }
        Ok(decls)
    }

    pub (super) fn parse_top_declaration(&mut self) -> anyhow::Result<TopDeclaration> {
        let tok = self.peek_next_token()?;
        match tok.value {
            TokenValue::Type => Ok(TopDeclaration::Type(self.parse_type_decl()?)),
            TokenValue::Data => Ok(TopDeclaration::Data(self.parse_data()?)),
            TokenValue::Newtype => Ok(TopDeclaration::Newtype(self.parse_newtype()?)),
            TokenValue::Class => Ok(TopDeclaration::Class(self.parse_class()?)),
            TokenValue::Instance => Ok(TopDeclaration::Instance(self.parse_instance()?)),
            TokenValue::Default => Ok(TopDeclaration::Default(self.parse_default_declaration()?)),
            TokenValue::Foreign => Ok(TopDeclaration::Foreign(self.parse_foreign_declaration()?)),
            _ => Ok(TopDeclaration::Declaration(self.parse_declaration(DeclKind::Normal)?))
        }
    }

    pub (super) fn parse_declaration(&mut self, decl_kind : DeclKind) -> anyhow::Result<Declaration> {
        unimplemented!()
    }

    pub (super) fn parse_default_declaration(&mut self) -> anyhow::Result<Vec<Type>> {
        unimplemented!()
    }
}