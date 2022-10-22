use super::{ParserState, ParseError};


#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    VarId,
    ConId,
    VarSym,
    ConSym,
    QVarId,
    QConId,
    QVarSym,
    QConSym,
    Integer,
    Float,
    Char,
    String,
    LeftParen,
    RightParen,
    Comma,
    Semicolon,
    VirtualSemiColon,
    LeftBracket,
    RightBracket,
    Backtick,
    LeftBrace,
    RightBrace,
    VirtualLeftBrace,
    VirtualRightBrace,
    Case,
    Class,
    Data,
    Default,
    Deriving,
    Do,
    Else,
    Foreign,
    If,
    Import,
    In,
    Infix,
    Infixl,
    Infixr,
    Instance,
    Let,
    Module,
    Newtype,
    Of,
    Then,
    Type,
    Where,
    Underscore,
    DotDot,
    Colon,
    DoubleColon,
    Equals,
    Backslash,
    Bar,
    LeftArrow,
    RightArrow,
    At,
    Tilde,
    DoubleArrow,
    Eof
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    token : TokenType,
    val : &'a str,
    loc : (usize, usize)
}

fn is_symbolic(c : char) -> bool {
    // TODO: Use Unicode properties. For now anything non-alphanumeric will do
    c == '!' || c == '#' || c == '$' || c == '%' || c == '&' || c == '*' ||
    c == '+' || c == '.' || c == '/' || c == '<' || c == '=' || c == '>' ||
    c == '?' || c == '@' || c == '\\' || c== '^' || c == '|' || c == '-' ||
    c == '~' || c == ':' || (!c.is_ascii() && !c.is_alphanumeric() && !c.is_whitespace())
} 

impl<'a> super::ParserState<'a> {
    pub (super) fn get_next_token(&mut self) -> Result<Token, ParseError> {   
        if let Some(tok) = self.queue.pop_front() {
            Ok(tok)
        } else {
            self.next_token()
        }
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        while let Some((p, c)) = self.chars.peek() {
            self.pos = *p;
            match c {
                '\n' => {
                    self.newlines.push(*p);
                    self.chars.next();
                },
                ws if c.is_whitespace() => {
                    self.chars.next();
                },
                '(' => self.queue_token(TokenType::LeftParen, 1)?,
                ')' => self.queue_token(TokenType::RightParen,1)?,
                '[' => self.queue_token(TokenType::LeftBracket,1)?,
                ']' => self.queue_token(TokenType::RightBracket,1)?,
                '`' => self.queue_token(TokenType::Backtick,1)?,
                ',' => self.queue_token(TokenType::Comma,1)?,
                ';' => self.queue_token(TokenType::Semicolon,1)?,
                '{' => {
                    self.chars.next();
                    if let Some((p, '-')) = self.chars.peek() {
                        self.read_block_comment()?;
                    } else {
                        self.queue_token(TokenType::LeftBrace, 1)?;
                    }
                },
                '}' => self.queue_token(TokenType::RightBrace,1)?,
                '"' => self.get_string()?,
                '\'' => self.get_char()?,
                x if c.is_numeric() => self.get_number()?,
                x if c.is_uppercase() => self.get_modcon()?,
                x if c.is_lowercase() => self.get_varid()?,
                x if is_symbolic(c) => self.get_symbol()?,
                _ => self.lex_error(format!("Lexing failed at {}", c).as_str())?
            }
            if let Some(tok) = self.queue.pop_front() {
                return Ok(tok);
            }
        }
        Ok(self.make_token(TokenType::Eof, 1))
    }

    fn get_string(&mut self) -> Result<(), ParseError> {
        self.chars.next();
        while let Some((p, c)) = self.chars.next() {
            match c {
                '"' => {
                    self.queue_token(TokenType::String, p - self.pos);
                    return Ok(());
                },
                '\\' => {
                    self.read_escape(TokenType::String);
                }
            }
        }
        self.lex_error("unterminated string")
    }

    fn read_escape(&mut self, tt : TokenType) -> Result<(), ParseError> {
        // As we're not parsing the strings or validating them, the only thing we need to bother
        // with here at the moment is making sure escaped terminators are skipped.
        // If we parsed the escapes we would have to pass a string back with the token.
        // And I don't want to do that even though this probably is the right place to 
        // do it.
        self.chars.next(); // skippity
        Ok(())
    }

    fn get_char(&mut self) -> Result<(), ParseError> {
        self.chars.next();
        if let Some((_, c)) = self.chars.next() {
            match c {
                '\\' => {
                    self.read_escape(TokenType::Char);
                }
            }
        }
        if let Some((p, '\'')) = self.chars.next() {
            self.queue_token(TokenType::Char, p - self.pos);
            return Ok(());
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn get_number(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn get_modcon(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn get_varid(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn get_symbol(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn line_comment_or_operator(&mut self) -> Option<Token> {
        unimplemented!()
    }

    fn get_id_token(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn read_block_comment(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn make_token(&self, token : TokenType, n : usize) -> Token {
        Token { token, val: &self.src[self.pos..self.pos+n], loc: (self.pos, self.pos+n)  }
    }

    fn queue_token(&self, token : TokenType, n : usize) -> Result<(), ParseError> {
        self.queue.push_back(self.make_token(token, n));
        Ok(())
    }

    fn lex_error<T>(&self, msg : &str) -> Result<T, ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.pos, self.pos) })
    }
}