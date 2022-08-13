use crate::lex;
use crate::Context;
use crate::error;

pub fn parse_file<'ctx>(context: &'ctx Context<'ctx>, filename: &str) {
    println!("Hello from the parser: {}", filename);

    let tokens = lex::lex_file(&context.sources, filename);

    match &tokens {
        Ok(ts) => println!("{:?}", ts),
        Err(e) => error::fatal_error(&e)
    }
}
