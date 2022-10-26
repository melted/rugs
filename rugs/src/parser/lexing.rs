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
            self.token_start = *p;
            let tok = match *c {
                '\n' => {
                    self.newlines.push(*p);
                    self.next();
                    None
                },
                _ if c.is_whitespace() => {
                    self.next(); 
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
                _ if is_symbolic(*c) => self.get_symbol()?,
                _ => {
                    let ch = *c;
                    self.lex_error(format!("Lexing failed at illegal char: {}", ch).as_str())?;
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
        self.next();
        while let Some((p, c)) = self.next() {
            match c {
                '"' => {
                    return Ok(self.token(Token::String(result)));
                },
                '\\' => {
                    if self.peek()?.is_whitespace() {
                        let end = self.snarf(|c| char::is_whitespace(*c));
                        if self.peek()? == '\'' {
                            self.advance(1);
                        } else {
                            return self.lex_error("Gap in string literal can only contain whitespace");
                        }
                    } else {
                        let c = self.read_escape(true)?;
                        result.push(c);
                    }
                }
                _ => result.push(c)
            }
        }
        self.lex_error("unterminated string")
    }

    fn get_char(&mut self) -> Result<Annotated<Token>, ParseError> {
        self.next();
        if let Some((_, c)) = self.next() {
            let ch = match c {
                '\\' => {
                    self.read_escape(false)?
                }
                c => c
            };
            if self.check_prefix("'") {
                self.advance(1);
                return Ok(self.token(Token::Char(ch)));
            }
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn read_escape(&mut self, for_string : bool) -> Result<char, ParseError> {
        unimplemented!()
    }


    fn get_number(&mut self) -> Result<Annotated<Token>, ParseError> {
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let stop = self.snarf(char::is_ascii_hexdigit)?;
            return Ok(self.token(Token::Integer(self.src[self.token_start..stop].to_string())));
        }
        if self.check_prefix("0o") || self.check_prefix("0O") {
            self.advance(2);
            let stop = self.snarf(|c| *c >= '0' && *c <= '7')?;
            return Ok(self.token(Token::Integer(self.src[self.token_start..stop].to_string())));
        }
        let mut stop = self.snarf(char::is_ascii_digit)?;
        let mut next = self.peek()?;
        if next == '.' || next == 'e' || next == 'E' {
            if next == '.' {
                self.advance(1);
                stop = self.snarf(char::is_ascii_digit)?;
                next = self.peek()?
            }
            if next == 'e' || next == 'E' {
                    self.advance(1);
                    let sign = self.peek()?;
                    if sign == '+' || sign == '-' {
                        self.advance(1);
                    }
                    stop = self.snarf(char::is_ascii_digit)?;
            }
            return Ok(self.token(Token::Float(self.src[self.token_start..stop].parse()?)));
        }
        Ok(self.token(Token::Integer(self.src[self.token_start..stop].to_string())))
    }

    fn get_modcon(&mut self) -> Result<Annotated<Token>, ParseError> {
        
        unimplemented!()
    }

    fn get_varid(&mut self) -> Result<Annotated<Token>, ParseError> {
        unimplemented!()
    }

    fn get_symbol(&mut self) -> Result<Option<Annotated<Token>>, ParseError> {
        unimplemented!()
    }

    fn read_block_comment(&mut self) -> Result<Annotated<Token>, ParseError> {
        unimplemented!()
    }

    fn snarf(&mut self, pred : impl Fn (&char) -> bool) -> Result<usize, ParseError> {
        let next = self.chars.peek();
        match next {
            None => return self.lex_error("Unexpected end of input"),
            Some((_, ch)) if !pred(ch) => return self.lex_error("Nothing to snarf"),
            _ => self.advance(1)
        };
        while let Some((_, ch)) = self.chars.peek() {
            if pred(ch) {
                self.next();
            } else {
                break;
            }
        }
        Ok(self.pos)
    }

    fn advance(&mut self, n : usize) {
        for _ in 0..n {
            self.next();
        }
    }

    fn check_prefix(&self, what: &str) -> bool {
        self.src[self.pos..].starts_with(what)
    }

    fn peek(&mut self) -> Result<char, ParseError> {
        if let Some((_, ch)) = self.chars.peek() {
            Ok(*ch)
        } else {
            self.lex_error("Unexpected end of file")
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        let item = self.chars.next();
        if let  Some((p, _)) = self.chars.peek() {
            self.pos = *p;
        }
        item
    }

    fn simple_token(&mut self, token : Token) -> Annotated<Token> {
        self.next();
        Annotated { annotations: Vec::new(), location:  (self.token_start, self.token_start+1), value: token }
    }

    fn token(&mut self, token : Token) -> Annotated<Token> {
        Annotated { annotations: Vec::new(), location:  (self.token_start, self.pos), value: token }
    }

    fn lex_error<T>(&self, msg : &str) -> Result<T, ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.token_start, self.pos) })
    }
}