use super::helpers::{varsym, error};
use super::{ParserState, ParseError};
use super::lexing::Token;
use crate::ast::*;


impl<'a> ParserState<'a> {
    
    pub (super) fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let exp = self.parse_infix_expression()?;
        if self.is_next(Token::DoubleColon)? {
            // TODO: Parse context and type
            Ok(typed(exp, Context::Mmm, Type::Na))
        } else {
            Ok(exp)
        }
    }

    fn parse_infix_expression(&mut self) -> Result<Expression, ParseError> {
        if self.is_next(varsym("-"))? {
            let exp = self.parse_expression()?;
            let args = vec![exp];
            return Ok(app(var("negate"), args));
        }
        let exp = self.parse_lexp()?;
        if let Some(op) = self.maybe_qop()? {
            let exp_right = self.parse_infix_expression()?;
            Ok(infix(op, exp, exp_right))
        } else {
            Ok(exp)
        }
    }

    fn maybe_qop(&mut self) -> Result<Option<Operator>, ParseError> {
        let op = match self.peek_next_token()?.value {
            Token::Colon =>
                Operator { module: None, constructor: true, ticked: false, name: ":".to_string() },
            Token::QConSym(module, con) =>
                Operator { module: Some(module), constructor: true, ticked: false, name: con },
            Token::QVarSym(module, op) =>
                Operator { module: Some(module), constructor: false, ticked: false, name: op },
            Token::ConSym(con) => 
                Operator { module: None, constructor: true, ticked: false, name: con },
            Token::VarSym(op) => 
                Operator { module: None, constructor: false, ticked: false, name: op },
            Token::Backtick => {
                let backtick = self.get_next_token()?;
                let op = match self.get_next_token()?.value {
                    Token::QVarId(module, var) =>
                        Operator { module: Some(module), constructor: false, ticked: true, name: var },
                    Token::QConId(module, con) =>
                        Operator { module: Some(module), constructor: true, ticked: true, name: con },
                    Token::VarId(var) =>
                        Operator { module: None, constructor: false, ticked: true, name: var },
                    Token::ConId(con) =>
                        Operator { module: None, constructor: true, ticked: true, name: con },
                    _ => return error("Expected identifier after backtick", backtick.location)
                };
                self.expect(Token::Backtick)?;
                return Ok(Some(op));
            },
            _ => return Ok(None)
        };
        self.get_next_token()?;
        Ok(Some(op))
    }

    fn parse_lexp(&mut self) -> Result<Expression, ParseError> {
        let tok = self.get_next_token()?;
        match tok.value {
            Token::Backslash => {
               //  let args = Vec::new();
                loop {

                }
            },
            Token::Let => {
                unimplemented!()
            },
            Token::If => {
                unimplemented!()
            },
            Token::Case => {
                unimplemented!()
            },
            Token::Do => {
                unimplemented!()
            },
            _ => {

            }
        }
        unimplemented!()
    }
    
    fn parse_fexp(&mut self) -> Result<Expression, ParseError> {
        unimplemented!()    
    }

    fn parse_aexp(&mut self) -> Result<Expression, ParseError> {
        unimplemented!()
    }


}