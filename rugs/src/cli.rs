use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about)]
pub struct Args {
    /// Dump the tokens from the lexer stage
    #[arg(long)]
    pub dump_tokens : bool,

    /// Input files
    pub file_name : Vec<String>,

    /// Load from stdin
    #[arg(long)]
    pub stdin : bool
}