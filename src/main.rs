mod lexer;
mod parser;

use std::{fs, sync::Arc};

use clap::Parser;
use lexer::lex::Lexer;
use parser::parse;

#[derive(Parser, Debug)]
#[command(
    author = "George Munyoro",
    version,
    about = "C Compiler",
    long_about = "C Compiler"
)]
struct Args {
    /// File to compile
    #[arg(short, long)]
    filename: String
}


fn main() {
    let args = Args::parse();
    let source = fs::read_to_string(args.filename.clone()).expect("Provided filepath should be readable.");

    let mut lx = Lexer::new(&source);
    let lexed_result = lx.tokenize();

    match lexed_result {
        Err(e) => {
            println!(
                "error: encountered token error:\n--> {}\n\n{e}",
                args.filename.clone()
            );
            println!("error: could not compile due to previous errors");
        }
        Ok(tokens) => {
            parse(Arc::new(tokens));
        }
    }
}
