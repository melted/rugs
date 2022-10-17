#![cfg(test)]

use super::parse;

#[test]
fn parse_empty_string() {
    let result = parse("");
    assert!(result.is_err(), "bah");
}
