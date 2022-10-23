use super::{ParserState, ParseError};
use crate::ast::Annotated;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    VarId(String),
    ConId(String),
    VarSym(String),
    ConSym(String),
    QVarId(String, String),
    QConId(String, String),
    QVarSym(String, String),
    QConSym(String, String),
    Integer(String),
    Float(f64),
    Char(char),
    String(String),
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

fn is_symbolic(c : char) -> bool {
    // TODO: Use Unicode properties. For now anything non-alphanumeric will do
    c == '!' || c == '#' || c == '$' || c == '%' || c == '&' || c == '*' ||
    c == '+' || c == '.' || c == '/' || c == '<' || c == '=' || c == '>' ||
    c == '?' || c == '@' || c == '\\' || c== '^' || c == '|' || c == '-' ||
    c == '~' || c == ':' || (!c.is_ascii() && !c.is_alphanumeric() && !c.is_whitespace())
}

impl<'a> super::ParserState<'a> {
    pub (super) fn get_next_token(&mut self) -> Result<Annotated<Token>, ParseError> {   
        if let Some(tok) = self.queue.pop_front() {
            Ok(tok)
        } else {
            self.next_token()
        }
    }

    fn next_token(&mut self) -> Result<Annotated<Token>, ParseError> {
        while let Some((p, c)) = self.chars.peek() {
            let ch = *c;
            let pos = *p;
            self.pos = *p;
            match ch {
                '\n' => {
                    self.newlines.push(*p);
                    self.chars.next();
                },
                ws if ch.is_whitespace() => {
                    self.chars.next();
                },
                '(' => self.queue_token(Token::LeftParen, 1)?,
                ')' => self.queue_token(Token::RightParen,1)?,
                '[' => self.queue_token(Token::LeftBracket,1)?,
                ']' => self.queue_token(Token::RightBracket,1)?,
                '`' => self.queue_token(Token::Backtick,1)?,
                ',' => self.queue_token(Token::Comma,1)?,
                ';' => self.queue_token(Token::Semicolon,1)?,
                '{' => {
                    self.chars.next();
                    if let Some((p, '-')) = self.chars.peek() {
                        self.read_block_comment()?;
                    } else {
                        self.queue_token(Token::LeftBrace, 1)?;
                    }
                },
                '}' => self.queue_token(Token::RightBrace,1)?,
                '"' => self.get_string()?,
                '\'' => self.get_char()?,
                x if ch.is_numeric() => self.get_number()?,
                x if ch.is_uppercase() => self.get_modcon()?,
                x if ch.is_lowercase() => self.get_varid()?,
                x if is_symbolic(ch) => self.get_symbol()?,
                _ => self.lex_error(format!("Lexing failed at {}", ch).as_str())?
            }
            if let Some(tok) = self.queue.pop_front() {
                return Ok(tok);
            }
        }
        Ok(self.make_token(Token::Eof, 1))
    }

    fn get_string(&mut self) -> Result<(), ParseError> {
        let mut result = String::new();
        self.chars.next();
        while let Some((p, c)) = self.chars.next() {
            match c {
                '"' => {
                    self.queue_token(Token::String(result), p - self.pos)?;
                    return Ok(());
                },
                '\\' => {
                    if let Some(c) = self.read_escape(true)? {
                        result.push(c);
                    }
                }
                _ => result.push(c)
            }
        }
        self.lex_error("unterminated string")
    }

    fn get_char(&mut self) -> Result<(), ParseError> {
        self.chars.next();
        if let Some((_, c)) = self.chars.next() {
            let ch = match c {
                '\\' => {
                    if let Some(c) = self.read_escape(false)? {
                        c
                    } else {
                        return self.lex_error("Whitespace escape in char literal isn't allowed.");
                    }
                }
                c => c
            };
            if let Some((p, '\'')) = self.chars.next() {
                self.queue_token(Token::Char(ch), p - self.pos)?;
                return Ok(());
            }
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn read_escape(&mut self, for_string : bool) -> Result<Option<char>, ParseError> {
        // As we're not parsing the strings or validating them, the only thing we need to bother
        // with here at the moment is making sure escaped terminators are skipped.
        // If we parsed the escapes we would have to pass a string back with the token.
        // And I don't want to do that even though this probably is the right place to 
        // do it.
        self.chars.next(); // skippity
        Ok(None)
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

    fn make_token(&mut self, token : Token, n : usize) -> Annotated<Token> {
        Annotated { annotations: Vec::new(), location:  (self.pos, self.pos+n), value: token }
    }

    fn queue_token(&mut self, token : Token, n : usize) -> Result<(), ParseError> {
        let tok = Annotated { annotations: Vec::new(), location:  (self.pos, self.pos+n), value: token };
        self.queue.push_back(tok);
        Ok(())
    }

    fn lex_error<T>(&self, msg : &str) -> Result<T, ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.pos, self.pos) })
    }
}