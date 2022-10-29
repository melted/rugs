use super::{lexing::Token, ParseError, ParserState};


impl<'a> ParserState<'a> {
    pub (super) fn expect(&mut self, t : Token) -> Result<(), ParseError> {
        let next = self.get_next_token()?;
        if t != next.value {
            Err(ParseError::new(format!("Expected {:?}, got {:?}", t, next.value).as_str(), next.location)) // TODO: Fix error location
        } else {
            Ok(())
        }
    }

    pub (super) fn is_next(&mut self, t : Token) -> Result<bool, ParseError> {
        let next = self.peek_next_token()?;
        if t == next.value {
            self.get_next_token(); // Swallow token
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub (super) fn error<T>(msg : &str, loc : Option<(usize, usize)>) -> Result<T, ParseError>{
    Err(ParseError::new(msg, loc))
} 

pub (super) fn varsym(op : &str) -> Token {
    Token::VarSym(op.to_string())
}

pub (super) fn varid(op : &str) -> Token {
    Token::VarId(op.to_string())
}