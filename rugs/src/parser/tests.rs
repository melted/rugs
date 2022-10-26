#![cfg(test)]

use crate::ast::Annotated;

use super::{parse, ParserState, ParseError};
use super::lexing::Token;

#[test]
fn parse_empty_string() {
    let result = parse("");
    assert!(result.is_err(), "bah");
}

fn get_tokens(state : &mut ParserState) -> Result<Vec<Annotated<Token>>, ParseError> {
    let mut out = Vec::new();
    loop {
        let t = state.get_next_token()?;
        let quit = t.value == Token::Eof;
        out.push(t);
        if quit {
            return Ok(out);
        }
    }
}

fn run_lexer(code : &str) -> Vec<Annotated<Token>> {
    let mut parse_state = ParserState::new(code);
    get_tokens(&mut parse_state).unwrap()
} 

// Lexer
#[test]
fn lex_empty() {
    let tokens = run_lexer("");
    assert!(tokens.len() == 1);
    assert_eq!(tokens[0].value, Token::Eof);
}

#[test]
fn lex_string() {
    let code = "\"hello this is a string\"";
    let tokens = run_lexer(code);
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0].value, Token::String("hello this is a string".to_string()));
    assert_eq!(tokens[1].value, Token::Eof)
}

