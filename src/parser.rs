use crate::lex;
use crate::source_resolver::Sources;
use crate::error;

pub fn parse_file<'ctx>(sources: &'ctx mut Sources, filename: &str) {
    println!("Hello from the parser: {}", filename);

    let tokens = lex::lex_file(sources, filename);

    match tokens {
        Ok(ts) => println!("{:?}", ts),
        Err(e) => error::fatal_error(&e)
    }
}
