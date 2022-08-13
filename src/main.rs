mod source_resolver;
mod lex;
mod parser;
mod error;
mod types;
mod namespace;

use source_resolver::Sources;
use typed_arena::Arena;

pub struct Context<'ctx> {
    pub sources: Sources,
    pub type_arena: Arena<types::Type<'ctx>>,
    pub symbol_arena: Arena<namespace::Symbol<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Self {
        Context {
            sources: Sources::new(),
            type_arena: Arena::new(),
            symbol_arena: Arena::new(),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let context = Context::new();

    if args.len() < 2 {
        eprintln!("Usage: ./lic [--noemit] [--llvm] <input.li>");
    } else {
        parser::parse_file(&context, args[1].as_str());
    }
}
