#![cfg(test)]

use super::lexing::{Token, TokenValue};
use super::{parse, ParserState, parse_expression};

#[test]
fn parse_empty_string() {
    let result = parse(None, "");
    assert!(result.is_ok());
}

fn get_tokens(state: &mut ParserState) -> anyhow::Result<Vec<Token>> {
    let mut out = Vec::new();
    loop {
        let t = state.get_next_token()?;
        let quit = t.value == TokenValue::Eof;
        out.push(t);
        if quit {
            return Ok(out);
        }
    }
}

fn run_lexer(code: &str) -> Vec<Token> {
    let mut parse_state = ParserState::new(code);
    get_tokens(&mut parse_state).unwrap()
}

// Lexer
#[test]
fn lex_empty() {
    let tokens = run_lexer("");
    assert!(tokens.len() == 3);
    assert_eq!(tokens[2].value, TokenValue::Eof);
}

#[test]
fn lex_string() {
    let code = "\"hello this is a string\"";
    let tokens = run_lexer(code);
    assert_eq!(tokens.len(), 4);
    assert_eq!(
        tokens[1].value,
        TokenValue::String("hello this is a string".to_string())
    );
    assert_eq!(tokens[3].value, TokenValue::Eof)
}

#[test]
fn parse_simple_expression() {
    let res = parse_expression("f 1");
    assert!(res.is_ok());
}

#[test]
fn parse_simple_expression2() {
    let res = parse_expression("1+1");
    assert!(res.is_ok());
}

#[test]
fn parse_decl() {
    let res = parse(None, "main = print 42");
    assert!(res.is_ok());
}

#[test]
fn parse_lambda() {
    let res = parse(None, "lam = \\x -> x + 1");
    assert!(res.is_ok());
}

#[test]
fn parse_let() {
    let res = parse(None, "mmmm = let y = x + 1 in f y");
    assert!(res.is_ok());
}

#[test]
fn parse_if() {
    let res = parse(None, "mmmm = if x then 1 else (7 + 8)");
    assert!(res.is_ok());
}

#[test]
fn parse_case() {
    let res = parse(None, r#"a = case x of
                                                                Just y -> 22
                                                                Nothing -> 33"#);
    assert!(res.is_ok());
}

#[test]
fn parse_data() {
    let res = parse(None,
                    r#"data Tree a = Node a (Tree a) (Tree a)
                                           | Leaf a"#);
    assert!(res.is_ok());
}


