use std::cmp::min;

use crate::{
    ast::*,
    support::{
        error::{self, RugsError},
        location::Location,
    },
};
use num_bigint::BigInt;
use num_traits::Num;

impl<'a> super::ParserState<'a> {
    pub(super) fn get_next_token(&mut self) -> error::Result<Token> {
        let tok = self.peek_next_token()?;
        self.token_pos += 1;
        Ok(tok)
    }

    pub(super) fn peek_next_token(&mut self) -> error::Result<Token> {
        assert!(self.token_pos <= self.tokens.len());
        while self.token_pos == self.tokens.len() {
            let tok = self.next_token()?;
            self.layout(tok)?;
        }
        Ok(self.tokens[self.token_pos].clone())
    }

    pub(super) fn rewind_lexer(&mut self, n: usize) {
        self.token_pos -= min(self.token_pos, n);
    }

    fn layout(&mut self, tok: Token) -> error::Result<()> {
        match tok.value {
            TokenValue::Let | TokenValue::Where | TokenValue::Do | TokenValue::Of => {
                self.layout_start = true;
                self.tokens.push(tok);
            }
            TokenValue::In => {
                let previous = &self.tokens[self.token_pos - 1].value;
                if previous != &TokenValue::RightBrace && previous != &TokenValue::VirtualRightBrace
                {
                    self.tokens.push(Token::new(TokenValue::VirtualRightBrace));
                    self.tokens.push(tok);
                    self.layout_stack.pop();
                }
            }
            TokenValue::StartLayout(n) => {
                self.tokens.push(Token::new(TokenValue::VirtualLeftBrace));
                let m = self.layout_stack.last().unwrap_or(&0);
                if n > *m {
                    self.layout_stack.push(n);
                } else {
                    self.tokens.push(Token::new(TokenValue::VirtualRightBrace));
                }
            }
            TokenValue::Indent(n) => loop {
                let m = self.layout_stack.last().unwrap_or(&0);
                if n < *m {
                    self.layout_stack.pop();
                    self.tokens.push(Token::new(TokenValue::VirtualRightBrace));
                } else {
                    if n == *m {
                        self.tokens.push(Token::new(TokenValue::Semicolon));
                    }
                    break;
                }
            },
            TokenValue::LeftBrace => {
                self.layout_stack.push(0);
            }
            TokenValue::RightBrace => {
                let concrete = self.layout_stack.pop();
                match concrete {
                    Some(0) => (),
                    _ => {
                        return self.lex_error(
                            "An explicit left brace must be matched with an explicit right brace",
                        )
                    }
                }
            }
            TokenValue::Eof => {
                for n in &self.layout_stack {
                    if *n == 0 {
                        return self.lex_error("Unterminated explicit left brace");
                    }
                    self.tokens.push(Token::new(TokenValue::VirtualRightBrace));
                }
                self.tokens.push(tok);
                self.layout_stack.clear();
            }
            _ => self.tokens.push(tok),
        }
        Ok(())
    }

    fn next_token(&mut self) -> error::Result<Token> {
        if let Some(tok) = self.queue.pop_front() {
            return Ok(tok);
        }
        while let Some((p, c)) = self.chars.peek() {
            self.token_start = *p;
            let tok = match *c {
                '\n' => {
                    self.indent = None;
                    self.metadata.newlines.push(*p);
                    self.next();
                    None
                }
                _ if c.is_whitespace() => {
                    self.next();
                    None
                }
                '(' => Some(self.simple_token(TokenValue::LeftParen)),
                ')' => Some(self.simple_token(TokenValue::RightParen)),
                '[' => Some(self.simple_token(TokenValue::LeftBracket)),
                ']' => Some(self.simple_token(TokenValue::RightBracket)),
                '`' => Some(self.simple_token(TokenValue::Backtick)),
                ',' => Some(self.simple_token(TokenValue::Comma)),
                ';' => Some(self.simple_token(TokenValue::Semicolon)),
                '{' => {
                    self.layout_start = false;
                    if self.check_prefix("{-") {
                        self.read_block_comment()?;
                        None
                    } else {
                        Some(self.simple_token(TokenValue::LeftBrace))
                    }
                }
                '}' => Some(self.simple_token(TokenValue::RightBrace)),
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
                let latest_newline = self.metadata.newlines.last().unwrap_or(&0);
                let indent = self.token_start - latest_newline;
                if self.layout_start {
                    self.layout_start = false;
                    self.queue.push_back(tok);
                    return Ok(self.token(TokenValue::StartLayout(indent + 1)));
                } else if self.indent.is_none() {
                    self.indent = Some(indent);
                    self.queue.push_back(tok);
                    return Ok(self.token(TokenValue::Indent(indent + 1)));
                }
                return Ok(tok);
            }
        }
        let eof = self.simple_token(TokenValue::Eof);
        if self.layout_start {
            self.layout_start = false;
            self.queue.push_back(eof);
            Ok(self.token(TokenValue::StartLayout(0)))
        } else {
            Ok(eof)
        }
    }

    fn get_string(&mut self) -> error::Result<Token> {
        let mut result = String::new();
        self.next();
        while let Some((_p, c)) = self.next() {
            match c {
                '"' => {
                    return Ok(self.token(TokenValue::String(result)));
                }
                '\\' => {
                    if self.peek()?.is_whitespace() {
                        let _end = self.snarf(|c| char::is_whitespace(*c));
                        if self.peek()? == '\'' {
                            self.advance(1);
                        } else {
                            return self
                                .lex_error("Gap in string literal can only contain whitespace");
                        }
                    }
                    if self.peek()? == '&' {
                        self.advance(1);
                    } else {
                        let c = self.read_escape()?;
                        result.push(c);
                    }
                }
                _ => result.push(c),
            }
        }
        self.lex_error("unterminated string")
    }

    fn get_char(&mut self) -> error::Result<Token> {
        self.next();
        if let Some((_, c)) = self.next() {
            let ch = match c {
                '\\' => self.read_escape()?,
                c => c,
            };
            if self.check_prefix("'") {
                self.advance(1);
                return Ok(self.token(TokenValue::Char(ch)));
            }
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn read_escape(&mut self) -> error::Result<char> {
        let asciis = [
            "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF",
            "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM",
            "SUB", "ESC", "FS", "GS", "RS", "US", "SP", "DEL",
        ];
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
                let v: u32 = ch.into();
                if (64..96).contains(&v) {
                    char::from((v - 64) as u8)
                } else {
                    return self.lex_error("Invalid ctrl escape");
                }
            }
            'o' => {
                return self.get_codepoint(|c| *c >= '0' && *c <= '7', 8);
            }
            'x' => {
                return self.get_codepoint(char::is_ascii_hexdigit, 16);
            }
            c if c.is_ascii_digit() => {
                return self.get_codepoint(char::is_ascii_digit, 10);
            }
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

    fn get_codepoint(&mut self, pred: impl Fn(&char) -> bool, radix: u32) -> error::Result<char> {
        let start = self.pos;
        let stop = self.snarf(pred)?;
        let code = match u32::from_str_radix(&self.src[start..stop], radix) {
            Ok(c) => c,
            Err(_) => return self.lex_error("Invalid codepoint escape"),
        };
        if let Some(ch) = char::from_u32(code) {
            Ok(ch)
        } else {
            self.lex_error("Invalid unicode codepoint in escape")
        }
    }

    fn get_number(&mut self) -> error::Result<Token> {
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let start = self.pos;
            let stop = self.snarf(char::is_ascii_hexdigit)?;
            let bigint = match BigInt::from_str_radix(&self.src[start..stop], 16) {
                Ok(c) => c,
                Err(_) => return self.lex_error("Invalid hex numeral"),
            };
            return Ok(self.token(TokenValue::Integer(bigint)));
        }
        if self.check_prefix("0o") || self.check_prefix("0O") {
            self.advance(2);
            let start = self.pos;
            let stop = self.snarf(|c| *c >= '0' && *c <= '7')?;
            let bigint = match BigInt::from_str_radix(&self.src[start..stop], 8) {
                Ok(c) => c,
                Err(_) => return self.lex_error("Invalid octal literal"),
            };
            return Ok(self.token(TokenValue::Integer(bigint)));
        }
        let mut stop = self.snarf(char::is_ascii_digit)?;
        let res = self.peek();
        if let Ok(mut next) = res {
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
                let float = match self.src[self.token_start..stop].parse() {
                    Ok(c) => c,
                    Err(_) => return self.lex_error("Invalid float literal"),
                };
                return Ok(self.token(TokenValue::Float(float)));
            }
        }
        let bigint = match BigInt::from_str_radix(&self.src[self.token_start..stop], 10) {
            Ok(c) => c,
            Err(_) => return self.lex_error("Invalid integer literal"),
        };
        Ok(self.token(TokenValue::Integer(bigint)))
    }

    fn get_modcon(&mut self) -> error::Result<Token> {
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
                            TokenValue::VarId(s) => Ok(self.token(TokenValue::QVarId(module, s))),
                            _ => self.lex_error("Invalid qualified name"),
                        }
                    }
                    c if is_symbolic(c) => {
                        let t = self.get_symbol()?;
                        if let Some(tok) = t {
                            match tok.value {
                                TokenValue::VarSym(s) => {
                                    Ok(self.token(TokenValue::QVarSym(module, s)))
                                }
                                TokenValue::ConSym(s) => {
                                    Ok(self.token(TokenValue::QConSym(module, s)))
                                }
                                _ => self.lex_error("Invalid qualified name"),
                            }
                        } else {
                            self.lex_error("Invalid qualified name")
                        }
                    }
                    _ => self.lex_error("Invalid qualified name"),
                }
            } else {
                Ok(self.token(TokenValue::QConId(
                    module,
                    self.src[last_dot + 1..self.pos].to_string(),
                )))
            }
        } else {
            Ok(self.token(TokenValue::ConId(
                self.src[self.token_start..self.pos].to_string(),
            )))
        }
    }

    fn get_varid(&mut self) -> error::Result<Token> {
        let start = self.pos;
        let end = self.snarf(|c| is_identifier_char(*c))?;
        let id = &self.src[start..end];
        match id {
            "case" => Ok(self.token(TokenValue::Case)),
            "class" => Ok(self.token(TokenValue::Class)),
            "data" => Ok(self.token(TokenValue::Data)),
            "default" => Ok(self.token(TokenValue::Default)),
            "deriving" => Ok(self.token(TokenValue::Deriving)),
            "do" => Ok(self.token(TokenValue::Do)),
            "else" => Ok(self.token(TokenValue::Else)),
            "foreign" => Ok(self.token(TokenValue::Foreign)),
            "if" => Ok(self.token(TokenValue::If)),
            "import" => Ok(self.token(TokenValue::Import)),
            "in" => Ok(self.token(TokenValue::In)),
            "infix" => Ok(self.token(TokenValue::Infix)),
            "infixl" => Ok(self.token(TokenValue::Infixl)),
            "infixr" => Ok(self.token(TokenValue::Infixr)),
            "instance" => Ok(self.token(TokenValue::Instance)),
            "let" => Ok(self.token(TokenValue::Let)),
            "module" => {
                self.layout_start = false;
                Ok(self.token(TokenValue::Module))
            }
            "newtype" => Ok(self.token(TokenValue::Newtype)),
            "of" => Ok(self.token(TokenValue::Of)),
            "then" => Ok(self.token(TokenValue::Then)),
            "type" => Ok(self.token(TokenValue::Type)),
            "where" => Ok(self.token(TokenValue::Where)),
            "_" => Ok(self.token(TokenValue::Underscore)),
            _ => Ok(self.token(TokenValue::VarId(id.to_string()))),
        }
    }

    fn get_symbol(&mut self) -> error::Result<Option<Token>> {
        let start = self.pos;
        let end = self.snarf(|c| is_symbolic(*c))?;
        let id = &self.src[start..end];
        match id {
            "--" => {
                // TODO: collect doc comments
                while let Some((_p, c)) = self.next() {
                    if c == '\n' {
                        break;
                    }
                }
                Ok(None)
            }
            ".." => Ok(Some(self.token(TokenValue::DotDot))),
            ":" => Ok(Some(self.token(TokenValue::Colon))),
            "::" => Ok(Some(self.token(TokenValue::DoubleColon))),
            "=" => Ok(Some(self.token(TokenValue::Equals))),
            "\\" => Ok(Some(self.token(TokenValue::Backslash))),
            "|" => Ok(Some(self.token(TokenValue::Bar))),
            "<-" => Ok(Some(self.token(TokenValue::LeftArrow))),
            "->" => Ok(Some(self.token(TokenValue::RightArrow))),
            "@" => Ok(Some(self.token(TokenValue::At))),
            "~" => Ok(Some(self.token(TokenValue::Tilde))),
            "=>" => Ok(Some(self.token(TokenValue::DoubleArrow))),
            _ => {
                if id.starts_with(':') {
                    Ok(Some(self.token(TokenValue::ConSym(id.to_string()))))
                } else {
                    Ok(Some(self.token(TokenValue::VarSym(id.to_string()))))
                }
            }
        }
    }

    fn read_block_comment(&mut self) -> error::Result<()> {
        let _start = self.pos;
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

    fn snarf(&mut self, pred: impl Fn(&char) -> bool) -> error::Result<usize> {
        let next = self.chars.peek();
        match next {
            None => return self.lex_error("Unexpected end of input"),
            Some((_, ch)) if !pred(ch) => return self.lex_error("Nothing to snarf"),
            _ => self.advance(1),
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

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    fn check_prefix(&self, what: &str) -> bool {
        self.src[self.pos..].starts_with(what)
    }

    fn peek(&mut self) -> error::Result<char> {
        if let Some((_, ch)) = self.chars.peek() {
            Ok(*ch)
        } else {
            self.lex_error("Unexpected end of file")
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        let item = self.chars.next();
        if let Some((p, _)) = self.chars.peek() {
            self.pos = *p;
        } else {
            self.pos = if let Some((p, _)) = item {
                p + 1
            } else {
                self.pos
            }
        }
        item
    }

    fn simple_token(&mut self, token: TokenValue) -> Token {
        self.next();
        self.token(token)
    }

    fn token(&mut self, token: TokenValue) -> Token {
        Token {
            value: token,
            location: self.location_current_token(),
        }
    }

    fn lex_error<T>(&self, msg: &str) -> error::Result<T> {
        Err(RugsError::Parse {
            msg: msg.to_string(),
            loc: self.location_current_token(),
        }
        .into())
    }

    fn location_current_token(&self) -> Location {
        // TODO: give InContext if applicable
        Location::Offset {
            start: self.token_start,
            end: self.pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
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
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub location: Location,
}

impl Token {
    pub(super) fn new(value: TokenValue) -> Token {
        Token {
            value,
            location: Location::Unlocated,
        }
    }

    pub(super) fn varsym(op: &str) -> Token {
        Token::new(TokenValue::VarSym(op.to_string()))
    }

    pub(super) fn varid(op: &str) -> Token {
        Token::new(TokenValue::VarId(op.to_string()))
    }
}

impl From<TokenValue> for Token {
    fn from(val: TokenValue) -> Self {
        Token::new(val)
    }
}

impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        self.value == other.value
    }
}

impl PartialEq<TokenValue> for Token {
    fn eq(&self, other: &TokenValue) -> bool {
        self.value == *other
    }
}

fn is_symbolic(c: char) -> bool {
    // TODO: Use Unicode properties. For now anything non-alphanumeric will do
    c == '!'
        || c == '#'
        || c == '$'
        || c == '%'
        || c == '&'
        || c == '*'
        || c == '+'
        || c == '.'
        || c == '/'
        || c == '<'
        || c == '='
        || c == '>'
        || c == '?'
        || c == '@'
        || c == '\\'
        || c == '^'
        || c == '|'
        || c == '-'
        || c == '~'
        || c == ':'
        || (!c.is_ascii() && !c.is_alphanumeric() && !c.is_whitespace())
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '\'' || c == '_'
}

impl TryFrom<Token> for Identifier {
    type Error = RugsError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match Self::try_from(value.value) {
            Ok(x) => Ok(x),
            Err(_) => Err(RugsError::Parse {
                msg: "Token can't be converted to identifier".to_string(),
                loc: Location::Unlocated,
            }),
        }
    }
}

impl TryFrom<TokenValue> for Identifier {
    type Error = RugsError;

    fn try_from(value: TokenValue) -> Result<Self, Self::Error> {
        match value {
            TokenValue::QConId(module, con) => Ok(qconid(&module, &con)),
            TokenValue::QVarId(module, var) => Ok(qvarid(&module, &var)),
            TokenValue::QConSym(module, sym) => Ok(qconsym(&module, &sym)),
            TokenValue::QVarSym(module, sym) => Ok(qvarsym(&module, &sym)),
            TokenValue::ConId(con) => Ok(conid(&con)),
            TokenValue::VarId(var) => Ok(varid(&var)),
            TokenValue::ConSym(con) => Ok(consym(&con)),
            TokenValue::VarSym(var) => Ok(varsym(&var)),
            _ => Err(RugsError::Parse {
                msg: "Token can't be converted to identifier".to_string(),
                loc: Location::Unlocated,
            }),
        }
    }
}
