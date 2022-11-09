use super::{ParserState, lexing::TokenValue};
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
        unimplemented!()
    }

    fn parse_import_decl(&mut self) -> anyhow::Result<Import> {
        unimplemented!()
    }

    fn parse_import(&mut self) -> anyhow::Result<Import> {
        unimplemented!()
    }
}