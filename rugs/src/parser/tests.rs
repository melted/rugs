#![cfg(test)]

use super::lexing::{Token, TokenValue};
use super::{parse, ParserState, parse_expression};

#[test]
fn parse_empty_string() {
    let result = parse(None, "");
    assert!(result.is_err(), "bah");
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
    assert!(tokens.len() == 1);
    assert_eq!(tokens[0].value, TokenValue::Eof);
}

#[test]
fn lex_string() {
    let code = "\"hello this is a string\"";
    let tokens = run_lexer(code);
    assert_eq!(tokens.len(), 2);
    assert_eq!(
        tokens[0].value,
        TokenValue::String("hello this is a string".to_string())
    );
    assert_eq!(tokens[1].value, TokenValue::Eof)
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
    dbg!(&res);
    assert!(res.is_ok());
}

#[test]
fn try_parse() {
    let mut parse_state = ParserState::new("ACon b c");
    let res = parse_state.try_parse(&mut |this| {
        let conid = this.parse_conid()?;
        let var = this.parse_var()?;
        Ok(conid)
    });
    dbg!(&parse_state);
}


#[test]
fn try_parse2() {
    let mut parse_state = ParserState::new("Abc 12 333 333 33 3 \n a b c D R");
    let res = parse_state.parse_conid();
    dbg!(&parse_state);
}