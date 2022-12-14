use super::{
    lexing::{Token, TokenValue},
    ParserState,
};
use crate::{ast::*, support::error};

impl<'a> ParserState<'a> {
    pub(super) fn parse_module(&mut self) -> error::Result<Module> {
        let mut this_module = Module::new();
        let module_name = if self.is_next(TokenValue::Module)? {
            let modid = self.parse_module_name()?;
            if let TokenValue::LeftParen = self.peek_next_token()?.value {
                let exports = self.parse_paren_list(&mut Self::parse_export)?;
                this_module.exports = Some(exports);
            }
            self.expect(TokenValue::Where)?;
            modid
        } else {
            module("Main")
        };
        this_module.name = module_name;
        self.parse_body(&mut this_module)?;
        Ok(this_module)
    }

    fn parse_body(&mut self, module: &mut Module) -> error::Result<()> {
        let brace = self.get_next_token()?;
        let is_virtual = match brace.value {
            TokenValue::LeftBrace => false,
            TokenValue::VirtualLeftBrace => true,
            _ => return Err(self.error("Missing brace at start of body")),
        };
        let tok = self.peek_next_token()?;
        let imports = match tok.value {
            TokenValue::Import => self.parse_import_declarations()?,
            _ => Vec::new(),
        };
        let decls = self.parse_top_declarations()?;
        if is_virtual {
            self.expect(TokenValue::VirtualRightBrace)?;
        } else {
            self.expect(TokenValue::RightBrace)?;
        }
        for d in &decls {
            module.names.extend(d.names()?.into_iter());
        }
        module.declarations = decls;
        module.imports = imports;
        Ok(())
    }

    fn parse_export(&mut self) -> error::Result<Export> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarId(_, _) | TokenValue::VarId(_) => {
                Ok(Export::Var(Identifier::try_from(tok)?))
            }
            TokenValue::QConId(_, _) | TokenValue::ConId(_) => {
                let id = Identifier::try_from(tok)?;
                let spec = self.parse_exposed_spec()?;
                Ok(Export::TypeOrClass(id, spec))
            }
            TokenValue::Module => {
                let tok = self.get_next_token()?;
                let modid = Identifier::try_from(tok)?;
                Ok(Export::Module(modid))
            }
            _ => Err(self.error("Invalid export")),
        }
    }

    fn parse_import_declarations(&mut self) -> error::Result<Vec<ImportDecl>> {
        let mut imports = Vec::new();
        loop {
            if self.is_next(TokenValue::Import)? {
                imports.push(self.parse_import_decl()?);
                let tok = self.peek_next_token()?;
                match tok.value {
                    TokenValue::Semicolon => {
                        self.get_next_token()?;
                    }
                    TokenValue::RightBrace | TokenValue::VirtualRightBrace => break,
                    _ => return Err(self.error("expected semicolon or right brace")),
                }
            } else {
                break;
            }
        }
        Ok(imports)
    }

    fn parse_import_decl(&mut self) -> error::Result<ImportDecl> {
        let qualified = self.is_next(Token::varid("qualified").value)?;
        let modid = self.parse_module_name()?;
        let mut import = self.new_import_decl(modid);
        import.qualified = qualified;
        if self.is_next(Token::varid("as").value)? {
            let alias = self.parse_module_name()?;
            import.alias = Some(alias);
        }
        let hiding = self.is_next(Token::varid("hiding").value)?;
        import.spec = if self.peek_next(TokenValue::LeftParen)? {
            let impspec = self.parse_paren_list(&mut Self::parse_import)?;
            if hiding {
                ImportSpec::Hide(impspec)
            } else {
                ImportSpec::Only(impspec)
            }
        } else if hiding {
            return Err(self.error("No list after `hiding`"));
        } else {
            ImportSpec::All
        };
        Ok(import)
    }

    fn parse_import(&mut self) -> error::Result<Import> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarId(_, _) | TokenValue::VarId(_) => {
                Ok(Import::Var(Identifier::try_from(tok)?))
            }
            TokenValue::QConId(_, _) | TokenValue::ConId(_) => {
                let id = Identifier::try_from(tok)?;
                let spec = self.parse_exposed_spec()?;
                Ok(Import::TypeOrClass(id, spec))
            }
            _ => Err(self.error("Invalid import")),
        }
    }

    fn parse_module_name(&mut self) -> error::Result<Identifier> {
        let mod_tok = self.get_next_token()?;
        match mod_tok.value {
            TokenValue::ConId(s) => Ok(module(&s)),
            TokenValue::QConId(m, s) => {
                let mut mid = m;
                mid.push('.');
                mid.push_str(&s);
                Ok(module(&mid))
            }
            _ => Err(self.error(&format!("Invalid module name token {:?}", mod_tok))),
        }
    }

    fn parse_exposed_spec(&mut self) -> error::Result<ExposedSpec> {
        if self.is_next(TokenValue::LeftParen)? {
            let tok = self.peek_next_token()?;
            match tok.value {
                TokenValue::DotDot => {
                    self.get_next_token()?;
                    self.expect(TokenValue::RightParen)?;
                    Ok(ExposedSpec::All)
                }
                _ => {
                    self.rewind_lexer(1);
                    let cons = self.parse_paren_list(&mut Self::parse_cname)?;
                    Ok(ExposedSpec::List(cons))
                }
            }
        } else {
            Ok(ExposedSpec::None)
        }
    }
}
