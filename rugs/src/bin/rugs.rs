use std::io;

use clap::Parser;
use rugs::cli::Args;
use rugs::parser::{dump_tokens, dump_ast_expression, dump_ast};
use rugs::session::Session;

fn main() -> Result<(), std::io::Error> {
    let args = Args::parse();
    let mut session = Session::new();
    if !args.file_names.is_empty() {
        session.load(args.file_names)?;
    }
    if args.stdin {
        session.load_stdin()?;
    }
    if args.dump_tokens {
        for (name, code) in &session.source {
            println!("{}\n---\n", name);
            let res = dump_tokens(code, &mut io::stdout().lock());
            println!("Result: {:?}\n", res);
        }
    } else if args.dump_ast {
        for (name, code) in &session.source {
            println!("{}\n---\n", name);
            let res = if args.expression {
                dump_ast_expression(code, &mut io::stdout().lock())
            } else {
                dump_ast(code, &mut io::stdout().lock())
            };
            println!("Result: {:?}\n", res);
        }
    } else {
        println!("Not implemented yet");
    }
    Ok(())
}

#[test]
fn test() {
    assert!(true);
}
