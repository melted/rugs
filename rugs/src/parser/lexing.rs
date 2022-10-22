use super::{ParserState, ParseError};



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

pub struct Token<'a> {
    token : TokenType,
    val : &'a str,
    loc : (usize, usize)
}

impl<'a> super::ParserState<'a> {
    pub (super) fn get_next_token(&mut self) -> Result<Token, ParseError> {
        if let Some(tok) = self.queue.pop_front() {
            return Ok(tok);
        } else {
            return self.next_token();
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
                x if c.is_numeric() =>
                    self.get_number()?,
                _ => {
                    self.get_id_token()?;
                }
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
        }
        unimplemented!()
    }


    fn get_char(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }

    fn get_number(&mut self) -> Result<(), ParseError> {
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

    fn lex_error(&self, msg : &str) -> Result<(), ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.pos, self.pos) })
    }
}