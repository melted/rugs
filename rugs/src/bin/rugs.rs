use std::io;

use rugs::parser::dump_tokens;


fn main() {
    let res = dump_tokens("module Hello.Hello where a = b 0x156 123e77 (\"a\\100bc\")", &mut io::stdout().lock());
    println!("{:?}", res);
}

#[test]
fn test() {
    assert!(true);
}