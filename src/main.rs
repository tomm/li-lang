mod source_resolver;
mod lex;
mod parser;
mod error;

use source_resolver::Sources;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut sources = Sources::new();

    if args.len() < 2 {
        eprintln!("Usage: ./lic [--noemit] [--llvm] <input.li>");
    } else {
        parser::parse_file(&mut sources, args[1].as_str());
    }
}
