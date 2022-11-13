

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

    pub (super) fn parse_type_signature(&mut self) -> anyhow::Result<Vec<Declaration>> {
        unimplemented!()
    }

    pub (super) fn parse_fixity_declaration(&mut self) -> anyhow::Result<Vec<Declaration>> {
        let assoc = match self.get_next_token()?.value {
            TokenValue::Infix => Association::NonAssociative,
            TokenValue::Infixl => Association::Left,
            TokenValue::Infixr => Association::Right,
            _ => return Err(self.error("Expected infix declaration"))
        };
        let prec = match self.peek_next_token()?.value {
            TokenValue::Integer(bn) => {
                let n : u32 = bn.try_into().unwrap_or(1000);
                if  n > 9 {
                    return Err(self.error("precedence must be betweem 0 and 9"));
                }
                n
            },
            _ => 9
        };
        let ops = self.parse_separated_by(&mut Self::parse_op, TokenValue::Comma)?;
        let output = ops.into_iter().map(
            |var| self.new_declaration(DeclarationValue::Fixity(var, assoc, prec)
            )).collect();
        Ok(output)
    }
}