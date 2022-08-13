use crate::types::*;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug,PartialEq,Eq)]
pub enum Symbol<'ctx> {
    Type(&'ctx Type<'ctx>),
}

pub struct Namespace<'ctx> {
    pub sym_order: Vec<(&'ctx str, &'ctx Symbol<'ctx>)>,
    pub sym_lookup: HashMap<&'ctx str, &'ctx Symbol<'ctx>>,
}

impl<'ctx> Namespace<'ctx> {
    pub fn add_symbol(self: &mut Self, context: &'ctx crate::Context<'ctx>, name: &'ctx str, symbol: Symbol<'ctx>) {
        match self.sym_lookup.entry(name) {
            Entry::Occupied(_) => {
                eprintln!("{} already in namespace. DIE!", name);
                panic!();
            },
            Entry::Vacant(e) => {
                let sym = context.symbol_arena.alloc(symbol);
                e.insert(sym);
                self.sym_order.push((name, sym));
            }
        }
    }
    pub fn new_global(context: &'ctx crate::Context<'ctx>) -> Self {
        let mut ns = Namespace {
            sym_order: vec![],
            sym_lookup: HashMap::new()
        };

        ns.add_symbol(&context, "unknown", Symbol::Type(context.type_arena.alloc(Type::Never)));
        ns.add_symbol(&context, "never", Symbol::Type(context.type_arena.alloc(Type::Never)));
        ns.add_symbol(&context, "void", Symbol::Type(context.type_arena.alloc(Type::Void)));
        ns.add_symbol(&context, "u8", Symbol::Type(context.type_arena.alloc(Type::U8)));
        ns.add_symbol(&context, "i8", Symbol::Type(context.type_arena.alloc(Type::I8)));
        ns.add_symbol(&context, "u16", Symbol::Type(context.type_arena.alloc(Type::U16)));
        ns.add_symbol(&context, "i16", Symbol::Type(context.type_arena.alloc(Type::I16)));

        ns
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lookup() {
        let context = crate::Context::new();
        let ns = Namespace::new_global(&context);

        assert_eq!(*ns.sym_lookup.get("u16").unwrap(), &Symbol::Type(&Type::U16));
        assert_eq!(ns.sym_order[5], ("u16", &Symbol::Type(&Type::U16)));
    }
}
