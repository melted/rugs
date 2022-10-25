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

fn is_identifier_char(c : char) -> bool {
    c.is_alphanumeric() || c == '\''
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
            self.pos = *p;
            let tok = match *c {
                '\n' => {
                    self.newlines.push(*p);
                    self.chars.next();
                    None
                },
                _ if c.is_whitespace() => {
                    self.chars.next(); 
                    None 
                },
                '(' => Some(self.simple_token(Token::LeftParen)),
                ')' => Some(self.simple_token(Token::RightParen)),
                '[' => Some(self.simple_token(Token::LeftBracket)),
                ']' => Some(self.simple_token(Token::RightBracket)),
                '`' => Some(self.simple_token(Token::Backtick)),
                ',' => Some(self.simple_token(Token::Comma)),
                ';' => Some(self.simple_token(Token::Semicolon)),
                '{' => {
                    if self.check_prefix("{-") {
                        self.read_block_comment()?;
                        None
                    } else {
                        Some(self.simple_token(Token::LeftBrace))
                    }
                },
                '}' => Some(self.simple_token(Token::RightBrace)),
                '"' => Some(self.get_string()?),
                '\'' => Some(self.get_char()?),
                _ if c.is_numeric() => Some(self.get_number()?),
                _ if c.is_uppercase() => Some(self.get_modcon()?),
                _ if c.is_lowercase() => Some(self.get_varid()?),
                _ if is_symbolic(*c) => Some(self.get_symbol()?),
                _ => {
                    let pos = *p;
                    let ch = *c;
                    self.lex_error(format!("Lexing failed at {} char:{}", pos,  ch).as_str())?;
                    None
                }
            }; 

            if let Some(tok) = tok {
                return Ok(tok);
            }
        }
        Ok(self.simple_token(Token::Eof))
    }

    fn get_string(&mut self) -> Result<Annotated<Token>, ParseError> {
        let mut result = String::new();
        self.chars.next();
        while let Some((p, c)) = self.chars.next() {
            match c {
                '"' => {
                    return Ok(self.token(Token::String(result), p));
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

    fn get_char(&mut self) -> Result<Annotated<Token>, ParseError> {
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
                return Ok(self.token(Token::Char(ch), p));
            }
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn read_escape(&mut self, for_string : bool) -> Result<Option<char>, ParseError> {
        // TODO: implement
        self.chars.next(); // skippity
        Ok(None)
    }


    fn get_number(&mut self) -> Result<Annotated<Token>, ParseError> {
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let stop = self.snarf(char::is_ascii_hexdigit);
            return Ok(self.token(Token::Integer(self.src[self.pos..stop].to_string()), stop));
        }
        if self.check_prefix("0o") || self.check_prefix("0O") {
            self.advance(2);
            let stop = self.snarf(|c| *c >= '0' && *c <= '7');
            return Ok(self.token(Token::Integer(self.src[self.pos..stop].to_string()), stop));
        }
        let stop = self.snarf(char::is_ascii_digit);
        if let Some((p, ch)) = self.chars.peek() {
            let mut is_float = false;
            let mut float_end = if *ch == '.' {
                is_float = true;
                self.advance(1);
                self.snarf(char::is_ascii_digit)
            } else {
                *p
            };
            if let Some((p, ch)) = self.chars.peek() {
                if *ch == 'e' || *ch == 'E' {
                    is_float = true;
                    self.advance(1);
                    if let Some((_, c)) = self.chars.peek() {
                        if *c == '+' || *c == '-' {
                            self.advance(1);
                        }
                    }
                    float_end = self.snarf(char::is_ascii_digit);
                }
            }
            if is_float {
                return Ok(self.token(Token::Float(self.src[self.pos..float_end].parse()?), float_end));
            }
        }
        Ok(self.token(Token::Integer(self.src[self.pos..stop].to_string()), stop))
    }

    fn get_modcon(&mut self) -> Result<Annotated<Token>, ParseError> {
        
        unimplemented!()
    }

    fn get_varid(&mut self) -> Result<Annotated<Token>, ParseError> {
        unimplemented!()
    }

    fn get_symbol(&mut self) -> Result<Annotated<Token>, ParseError> {
        unimplemented!()
    }

    fn read_block_comment(&mut self) -> Result<Annotated<Token>, ParseError> {
        unimplemented!()
    }

    fn snarf(&mut self, pred : impl Fn (&char) -> bool) -> usize {
        while let Some((p, ch)) = self.chars.peek() {
            if pred(ch) {
                self.chars.next();
            } else {
                return *p;
            }
        }
        self.pos
    }

    fn advance(&mut self, n : usize) {
        for _ in 0..n {
            self.chars.next();
        }
    }

    fn check_prefix(&self, what: &str) -> bool {
        self.src[self.pos..].starts_with(what)
    }

    fn simple_token(&mut self, token : Token) -> Annotated<Token> {
        self.chars.next();
        Annotated { annotations: Vec::new(), location:  (self.pos, self.pos+1), value: token }
    }

    fn token(&mut self, token : Token, to : usize) -> Annotated<Token> {
        Annotated { annotations: Vec::new(), location:  (self.pos, to), value: token }
    }

    fn lex_error<T>(&self, msg : &str) -> Result<T, ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.pos, self.pos) })
    }
}