use std::io;

use rugs::parser::dump_tokens;


fn main() {
    let res = dump_tokens("(\"abc\")", &mut io::stdout().lock());
    println!("{:?}", res);
}

#[test]
fn test() {
    assert!(true);
}