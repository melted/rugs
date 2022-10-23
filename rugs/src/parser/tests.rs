#![cfg(test)]

use super::{parse, ParserState};
use super::lexing::Token;

#[test]
fn parse_empty_string() {
    let result = parse("");
    assert!(result.is_err(), "bah");
}

// Lexer
#[test]
fn lex_empty() {
    let code = "";
    let mut parse_state = ParserState::new(code);
    let token = parse_state.get_next_token().unwrap();
    assert_eq!(*token, Token::Eof);
}