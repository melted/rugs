use super::helpers::varsym;
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
        let tok = self.peek_next_token()?;
        let op = match tok.value {
            Token::QVarSym(module, symb) => module + "." + &symb,
            Token::VarSym(symb) => symb, 
            _ => return Ok(exp)
        };
        let exp_right = self.parse_infix_expression()?;
        Ok(infix(&op, exp, exp_right))
    }

    fn parse_lexp(&mut self) -> Result<Expression, ParseError> {
        let tok = self.get_next_token()?;
        match tok.value {
            Token::Backslash => {
               // let args = Vec::new();
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }

    
    fn parse_lpattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }

    
    fn parse_apattern(&mut self) -> Result<Pattern, ParseError> {
        unimplemented!()
    }
}