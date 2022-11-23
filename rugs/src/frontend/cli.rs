use clap::Parser;

#[derive(Parser)]
#[command(author, version, about)]
pub struct Args {
    /// Dump the tokens from the lexer stage
    #[arg(long)]
    pub dump_tokens: bool,

    /// Dump the parser output
    #[arg(long)]
    pub dump_ast: bool,

    /// The log level
    #[arg(long)]
    pub log_level: Option<u32>,

    /// The input is an Haskell expression 
    #[arg(long, short='e')]
    pub expression: bool,

    /// Input files
    pub file_names: Vec<String>,

    /// Load from stdin
    #[arg(long)]
    pub stdin: bool,
}
