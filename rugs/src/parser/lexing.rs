use num_bigint::BigInt;
use num_traits::Num;
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
    Integer(BigInt),
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
    StartLayout(usize),
    Indent(usize),
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
    c.is_alphanumeric() || c == '\'' || c == '_'
}

fn virtual_token(t : Token) -> Annotated<Token> {
    Annotated { annotations: Vec::new(), location: None, value: t }
}

impl<'a> super::ParserState<'a> {
    pub (super) fn get_next_token(&mut self) -> Result<Annotated<Token>, ParseError> {   
        let tok =  self.next_token()?;
        self.check_layout_start(&tok);
        self.layout(tok)
    }

    fn check_layout_start(&mut self, tok: &Annotated<Token>) {
        match tok.value {
            Token::Let | Token::Where | Token::Do | Token::Of => { self.layout_start = true; }
            _ => ()
        }
    }

    fn layout(&mut self, tok: Annotated<Token>) -> Result<Annotated<Token>, ParseError> {
        match tok.value {
            Token::StartLayout(n) => {
                let m = self.layout_stack.last().unwrap_or(&0);
                if n > *m {
                    self.layout_stack.push(n);
                } else {
                    let token = virtual_token(Token::VirtualRightBrace);
                    self.queue.push(token);
                }
                return Ok(virtual_token(Token::VirtualLeftBrace));
            },
            Token::Indent(n) => {
                let m = self.layout_stack.last().unwrap_or(&0);
                if *m == n {
                    return Ok(virtual_token(Token::VirtualSemiColon));
                } else if n < *m {
                    while self.layout_stack.last().map_or(false,|m| n<*m ) {
                        self.layout_stack.pop();
                        let token = virtual_token(Token::VirtualRightBrace);
                        self.queue.push(token);
                    }
                    return Ok(self.queue.pop().unwrap()); // YOLO
                } else {
                    return self.get_next_token() // Recursing should be fine as tailcall
                }
            },
            Token::LeftBrace => {
                self.layout_stack.push(0);
            },
            Token::RightBrace => {
                let concrete = self.layout_stack.pop();
                match concrete {
                    Some(0) => (),
                    _ => return self.lex_error("An explicit left brace must be matched with an explicit right brace")
                }
            },
            Token::Eof => {
                self.queue.push(tok);
                for n in &self.layout_stack {
                    if *n == 0 {
                        return self.lex_error("Unterminated explicit left brace");
                    }
                    self.queue.push(virtual_token(Token::VirtualRightBrace));
                }
                self.layout_stack.clear();
                return Ok(self.queue.pop().unwrap());
            }
            _ => {}
        }
        Ok(tok)
    }

    fn next_token(&mut self) -> Result<Annotated<Token>, ParseError> {
        if let Some(tok) = self.queue.pop() {
            return Ok(tok);
        }
        while let Some((p, c)) = self.chars.peek() {
            self.token_start = *p;
            let tok = match *c {
                '\n' => {
                    self.indent = None;
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
                    self.layout_start = false;
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
                _ if c.is_lowercase() || *c == '_' => Some(self.get_varid()?),
                _ if is_symbolic(*c) => self.get_symbol()?,
                _ => {
                    let ch = *c;
                    self.lex_error(format!("Lexing failed at illegal char: {}", ch).as_str())?;
                    None
                }
            };

            if let Some(tok) = tok {
                let latest_newline = self.newlines.last().unwrap_or(&0);
                let indent = self.token_start - latest_newline;
                if self.layout_start {
                    self.layout_start = false;
                    self.queue.push(tok);
                    return Ok(self.token(Token::StartLayout(indent+1)));
                } else if self.indent.is_none() {
                    self.indent = Some(indent);
                    self.queue.push(tok);
                    return Ok(self.token(Token::Indent(indent+1)));
                }
                return Ok(tok);
            }
        }
        let eof = self.simple_token(Token::Eof);
        if self.layout_start {
            self.layout_start = false;
            self.queue.push(eof);
            Ok(self.token(Token::StartLayout(0)))
        } else {
            Ok(eof)
        }
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
                    } if self.peek()? == '&' {
                        self.advance(1);
                    } else {
                        let c = self.read_escape()?;
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
                    self.read_escape()?
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

    fn read_escape(&mut self) -> Result<char, ParseError> {
        let asciis = ["NUL", "SOH",  "STX", "ETX", "EOT",
        "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF" ,"CR" ,"SO" ,"SI" ,"DLE",
        "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN",
        "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP", "DEL"];
        let ch = match self.peek()? {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            '\\' => '\\',
            '\"' => '"',
            '\'' => '\'',
            '^' => {
                self.advance(1);
                let ch = self.peek()?;
                let v : u32 = ch.into();
                if v >= 64 && v < 96 {
                    char::from((v - 64) as u8)
                } else {
                    return self.lex_error("Invalid ctrl escape");
                }
            },
            'o' => {
                return self.get_codepoint(|c| *c >= '0' && *c <= '7', 8);
            },
            'x' => {
                return self.get_codepoint(char::is_ascii_hexdigit, 16);
            },
            c if c.is_ascii_digit() => {
                return self.get_codepoint(char::is_ascii_digit, 10);
            },
            _ => {
                for (i, s) in asciis.into_iter().enumerate() {
                    if self.check_prefix(s) {
                        self.advance(s.len());
                        let c = char::from(i as u8); 
                        return Ok(c);
                    }
                }
                return self.lex_error("Invalid escape in literal");
            }
        };
        self.advance(1);
        Ok(ch)
    }

    fn get_codepoint(&mut self, pred : impl Fn (&char) -> bool, radix : u32) -> Result<char, ParseError> {
        let start = self.pos;
        let stop = self.snarf(pred)?;
        let code = u32::from_str_radix(&self.src[start..stop], radix)?;
        if let Some(ch) = char::from_u32(code) {
            Ok(ch)
        } else {
            self.lex_error("Invalid unicode codepoint in escape")
        }
    }

    fn get_number(&mut self) -> Result<Annotated<Token>, ParseError> {
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let start = self.pos;
            let stop = self.snarf(char::is_ascii_hexdigit)?;
            let bigint = BigInt::from_str_radix(&self.src[start..stop], 16)?;
            return Ok(self.token(Token::Integer(bigint)));
        }
        if self.check_prefix("0o") || self.check_prefix("0O") {
            self.advance(2);
            let start = self.pos;
            let stop = self.snarf(|c| *c >= '0' && *c <= '7')?;
            let bigint = BigInt::from_str_radix(&self.src[start..stop], 8)?;
            return Ok(self.token(Token::Integer(bigint)));
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
        let bigint = BigInt::from_str_radix(&self.src[self.token_start..stop], 10)?;
        Ok(self.token(Token::Integer(bigint)))
    }

    fn get_modcon(&mut self) -> Result<Annotated<Token>, ParseError> {
        let mut qualified = false;
        let mut not_modid = false;
        let mut last_dot = self.pos;
        loop {
            self.snarf(|c| is_identifier_char(*c))?;
            if let Some((p, '.')) = self.chars.peek() {
                qualified = true;
                last_dot = *p;
                self.advance(1);
                if !char::is_uppercase(self.peek()?) {
                    not_modid = true;
                    break;
                }
            } else {
                break;
            }
        }
        if qualified {
            let module = self.src[self.token_start..last_dot].to_string();
            if not_modid {
                match self.peek()? {
                    c if c.is_lowercase() || c == '_' => {
                        let t = self.get_varid()?;
                        match t.value {
                            Token::VarId(s) => Ok(self.token(Token::QVarId(module, s))),
                            _ => self.lex_error("Invalid qualified name")
                        }
                    },
                    c if is_symbolic(c) => {
                        let t = self.get_symbol()?;
                        if let Some(tok) = t {
                            match tok.value {
                                Token::VarSym(s) => Ok(self.token(Token::QVarSym(module, s))),
                                Token::ConSym(s) => Ok(self.token(Token::QConSym(module, s))),
                                _ => self.lex_error("Invalid qualified name")
                            }
                        } else {
                            self.lex_error("Invalid qualified name")
                        }
                    },
                    _ => {
                        self.lex_error("Invalid qualified name")
                    }
                }
            } else {
                Ok(self.token(Token::QConId(module, self.src[last_dot+1..self.pos].to_string())))
            }
        } else {
            Ok(self.token(Token::ConId(self.src[self.token_start..self.pos].to_string())))
        }
    }

    fn get_varid(&mut self) -> Result<Annotated<Token>, ParseError> {
        let start = self.pos;
        let end = self.snarf(|c| is_identifier_char(*c))?;
        let id = &self.src[start..end];
        match id {
            "case" => Ok(self.token(Token::Case)),
            "class" => Ok(self.token(Token::Class)),
            "data" => Ok(self.token(Token::Data)),
            "default" => Ok(self.token(Token::Default)),
            "deriving" => Ok(self.token(Token::Deriving)),
            "do" => Ok(self.token(Token::Do)),
            "else" => Ok(self.token(Token::Else)),
            "foreign" => Ok(self.token(Token::Foreign)),
            "if" => Ok(self.token(Token::If)),
            "import" => Ok(self.token(Token::Import)),
            "in" => Ok(self.token(Token::In)),
            "infix" => Ok(self.token(Token::Infix)),
            "infixl" => Ok(self.token(Token::Infixl)),
            "infixr" => Ok(self.token(Token::Infixr)),
            "instance" => Ok(self.token(Token::Instance)),
            "let" => Ok(self.token(Token::Let)),
            "module" => Ok(self.token(Token::Module)),
            "newtype" => Ok(self.token(Token::Newtype)),
            "of" => Ok(self.token(Token::Of)),
            "then" => Ok(self.token(Token::Then)),
            "type" => Ok(self.token(Token::Type)),
            "where" => Ok(self.token(Token::Where)),
            "_" => Ok(self.token(Token::Underscore)),
            _ => Ok(self.token(Token::VarId(id.to_string())))
        }
    }

    fn get_symbol(&mut self) -> Result<Option<Annotated<Token>>, ParseError> {
        let start = self.pos;
        let end = self.snarf(|c| is_symbolic(*c))?;
        let id = &self.src[start..end];
        match id {
            "--" => {
                while let Some((p, c)) = self.next() {
                    if c == '\n' {
                        break;
                    }
                }
                Ok(None)
            }
            ".." => Ok(Some(self.token(Token::DotDot))),
            ":" => Ok(Some(self.token(Token::Colon))),
            "::" => Ok(Some(self.token(Token::DoubleColon))),
            "=" => Ok(Some(self.token(Token::Equals))),
            "\\" => Ok(Some(self.token(Token::Backslash))),
            "|" => Ok(Some(self.token(Token::Bar))),
            "<-" => Ok(Some(self.token(Token::LeftArrow))),
            "->" => Ok(Some(self.token(Token::RightArrow))),
            "@" => Ok(Some(self.token(Token::At))),
            "~" => Ok(Some(self.token(Token::Tilde))),
            "=>" => Ok(Some(self.token(Token::DoubleArrow))),
            _ => {
                if id.starts_with(':') {
                    Ok(Some(self.token(Token::ConSym(id.to_string()))))
                } else {
                    Ok(Some(self.token(Token::VarSym(id.to_string()))))
                }
            }
        }
    }

    fn read_block_comment(&mut self) -> Result<(), ParseError> {
        let start = self.pos;
        // TODO: Collect pragmas and doc comments.
        if self.check_prefix("{-#") {
            // It's a pragma
        }
        self.advance(2);
        loop {
            if self.check_prefix("-}") {
                break;
            }
            if self.check_prefix("{-") {
                self.read_block_comment()?
            }
            let x = self.next();
            if x.is_none() {
                return self.lex_error("Unterminated block comment");
            }
        }
        self.advance(2);
        Ok(())
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
        self.token(token)
    }

    fn token(&mut self, token : Token) -> Annotated<Token> {
        Annotated { annotations: Vec::new(), location:  Some((self.token_start, self.pos)), value: token }
    }

    fn lex_error<T>(&self, msg : &str) -> Result<T, ParseError> {
        Err(ParseError { msg: msg.to_string(), loc: (self.token_start, self.pos) })
    }
}