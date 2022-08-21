mod ast;
mod error;
mod lex;
mod namespace;
mod parser;
mod source_resolver;
mod types;

use source_resolver::Sources;
use typed_arena::Arena;

pub type Ctx<'ctx> = &'ctx Context<'ctx>;

pub struct Context<'ctx> {
    pub sources: Sources,
    type_arena: Arena<types::Type<'ctx>>,
    symbol_arena: Arena<namespace::Symbol<'ctx>>,
    astnode_arena: Arena<ast::AstNode<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Self {
        Context {
            sources: Sources::new(),
            type_arena: Arena::new(),
            symbol_arena: Arena::new(),
            astnode_arena: Arena::new(),
        }
    }

    pub fn alloc_type(self: &'ctx Self, t: types::Type<'ctx>) -> &'ctx mut types::Type<'ctx> {
        self.type_arena.alloc(t)
    }

    pub fn alloc_symbol(
        self: &'ctx Self,
        s: namespace::Symbol<'ctx>,
    ) -> &'ctx mut namespace::Symbol<'ctx> {
        self.symbol_arena.alloc(s)
    }

    pub fn alloc_astnode(self: &'ctx Self, n: ast::AstNode<'ctx>) -> &'ctx mut ast::AstNode<'ctx> {
        self.astnode_arena.alloc(n)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let context = Context::new();

    if args.len() < 2 {
        eprintln!("Usage: ./lic [--noemit] [--llvm] <input.li>");
    } else {
        match parser::parse_file(&context, args[1].as_str()) {
            Ok(()) => {}
            Err(e) => crate::error::fatal_error(&e),
        }
    }
}
