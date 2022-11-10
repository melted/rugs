use super::{ParserState, lexing::{TokenValue, Token}};
use crate::{ast::*};
use super::helpers::error;


impl<'a> ParserState<'a> {
    pub (super) fn parse_module(&mut self) -> anyhow::Result<Module> {
        let mut this_module = Module::new();
        let module_name = if self.is_next(TokenValue::Module)? {
            let mod_str = self.get_next_token()?;
            let modid = match mod_str.value {
                TokenValue::ConId(s) => s,
                TokenValue::QConId(m, s) => {
                    let mut mid = m.clone();
                    mid.push('.');
                    mid.push_str(&s);
                    mid
                }
                _ => return error(&format!("Invalid module name token {:?}", mod_str), mod_str.location)
            };
            if let TokenValue::LeftParen = self.peek_next_token()?.value {
                let exports = self.parse_paren_list(Self::parse_export)?;
                this_module.exports = Some(exports);
            } else {
                this_module.exports = None;
            }
            self.expect(TokenValue::Where)?;
            modid
        } else {
            "Main".to_string()
        };
        this_module.name = module_name;
        self.parse_body(&mut this_module)?;
        Ok(this_module)
    }

    fn parse_body(&mut self, module : &mut Module) -> anyhow::Result<()> {
        unimplemented!()
    }

    fn parse_export(&mut self) -> anyhow::Result<Export> {
        let tok = self.get_next_token()?;
        match tok.value {
            TokenValue::QVarId(_, _) | TokenValue::VarId(_) 
                => Ok(Export::Var(Identifier::try_from(tok)?)),
            TokenValue::QConId(_, _) | TokenValue::ConId(_) => {
                if self.is_next(TokenValue::LeftParen)? {
                    let tok = self.peek_next_token()?;
                    match tok.value {
                        TokenValue::DotDot => {
                            self.get_next_token()?;
                            self.expect(TokenValue::RightParen)?;
                            Ok(Export::Type(ExposedSpec::All))
                        },
                        _ => {
                            let mut is_tycls : Option<bool> = None;
                            self.push_token(TokenValue::LeftParen.into());
                            let cons = self.parse_paren_list(|this| {
                                let tok = self.get_next_token()?;
                                if let Some(b) = is_tycls {
                                    match tok.value {
                                        TokenValue::QConId(_,_) | TokenValue::ConId(_) if !b
                                            => Ok(Identifier::try_from(tok)?),
                                        TokenValue::QVarId(_, _) | TokenValue::VarId(_) if b
                                            => Ok(Identifier::try_from(tok)?),
                                        _ => error("Invalid export", tok.location)
                                    }
                                } else {
                                    match tok.value {
                                        TokenValue::QConId(_,_) | TokenValue::ConId(_) => {
                                                is_tycls = Some(false);
                                                Ok(Identifier::try_from(tok)?)
                                            },
                                        TokenValue::QVarId(_, _) | TokenValue::VarId(_) => {
                                            is_tycls = Some(true);
                                            Ok(Identifier::try_from(tok)?)
                                        },
                                        _ => error("Invalid export", tok.location)
                                    }
                                }
                            })?;
                            match is_tycls {
                                Some(true) => Ok(Export::Class(ExposedSpec::List(cons))),
                                Some(false) => Ok(Export::Type(ExposedSpec::List(cons))),
                                None => Ok(Export::Type(ExposedSpec::List(cons)))
                            }
                        }
                    }
                } else {
                    Ok(Export::Type(ExposedSpec::None))
                }
            },
            TokenValue::Module => {
                let tok = self.get_next_token()?;
                let modid = Identifier::try_from(tok.value)?;
                Ok(Export::Module(modid))
            },
            _ => error("Invalid export", tok.location)
        }
    }

    fn parse_import_decl(&mut self) -> anyhow::Result<Import> {
        unimplemented!()
    }

    fn parse_import(&mut self) -> anyhow::Result<Import> {
        unimplemented!()
    }
}