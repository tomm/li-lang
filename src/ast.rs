use crate::lex::TokenLoc;
use crate::types::Type;
use std::cell::Cell;

pub type AstNodeRef<'ctx> = &'ctx AstNode<'ctx>;
pub type AstNodeMutRef<'ctx> = &'ctx mut AstNode<'ctx>;
type TypeRef<'ctx> = &'ctx Type<'ctx>;

#[derive(Debug)]
pub struct AstNode<'ctx> {
    pub loc: TokenLoc<'ctx>,
    pub eval_type: Cell<Option<TypeRef<'ctx>>>,
    pub node_type: AstNodeType<'ctx>,
}

impl<'ctx> AstNode<'ctx> {
    pub fn new(loc: TokenLoc<'ctx>, node_type: AstNodeType<'ctx>) -> AstNode<'ctx> {
        AstNode {
            eval_type: Cell::new(None),
            node_type,
            loc,
        }
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn pretty_print(self: &Self, indent: i32) {
        macro_rules! indent {
            () => {
                for _ in 0..indent {
                    print!("\t");
                }
            };
        }
        indent!();
        match &self.node_type {
            AstNodeType::Module(contents) => {
                println!("module");
                for i in contents {
                    i.pretty_print(indent + 1);
                }
            }
            AstNodeType::StaticVar {
                is_const,
                name,
                typespec,
            } => {
                println!(
                    "{} {}: {:?}",
                    if *is_const { "const" } else { "var" },
                    name,
                    typespec
                );
            }
            AstNodeType::Func {
                name,
                ref args,
                ref ret,
                body,
            } => {
                println!("fn {} {:?} -> {:?}", name, args, ret);
                if let Some(body) = body {
                    body.pretty_print(indent + 1);
                } else {
                    indent!();
                    print!("\t");
                    println!("<fwd def>");
                }
            }
            AstNodeType::Asm(asm_text) => println!("asm('{}')", asm_text),
            AstNodeType::ExprList(exprs) => {
                println!("ExprList");
                for expr in exprs {
                    expr.pretty_print(indent + 1);
                }
            }
            AstNodeType::ExprVoidLiteral => println!("void literal"),
            AstNodeType::ExprLocalScope {
                var_name,
                var_type,
                value,
                scoped_expr,
            } => {
                println!("localscope var {}: {:?}", var_name, var_type);
                scoped_expr.pretty_print(indent + 1);
            }
            AstNodeType::ExprIdent(ident) => println!("ident ({})", ident),
            AstNodeType::ExprBinop { op, args } => {
                println!("op {:?}", op);
                args[0].pretty_print(indent + 1);
                args[1].pretty_print(indent + 1);
            }
            AstNodeType::ExprIfElse {
                condition,
                on_true,
                on_false,
            } => {
                println!("if");
                condition.pretty_print(indent + 1);
                indent!();
                println!("then");
                on_true.pretty_print(indent + 1);
                indent!();
                println!("else");
                on_false.pretty_print(indent + 1);
            }
            AstNodeType::ExprLoop {
                label,
                condition,
                body,
                on_next_iter,
            } => {
                if let Some(l) = label {
                    print!("'{} ", l);
                }
                if let Some(cond) = condition {
                    println!("loop while:");
                    cond.pretty_print(indent + 1);
                } else {
                    println!("loop (forever)");
                }
                if let Some(on_next_iter) = on_next_iter {
                    indent!();
                    println!("loop on_next_iter action:");
                    on_next_iter.pretty_print(indent + 1);
                }
                indent!();
                println!("loop body:");
                body.pretty_print(indent + 1);
            }
            AstNodeType::StructDecl { name, members } => {
                println!("struct {:?}", name);
                for m in members {
                    indent!();
                    println!("\t{}: {:?}", m.0, m.1);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Assign,
}

#[derive(Debug)]
pub struct AstFnArg<'ctx> {
    pub name: &'ctx str,
    pub type_: TypeSpecifier<'ctx>,
}

#[derive(Debug)]
pub enum TypeSpecifier<'ctx> {
    NamedType(&'ctx str),
    Array(Box<TypeSpecifier<'ctx>>, usize /* elems */),
    Func(Vec<TypeSpecifier<'ctx>>, Option<Box<TypeSpecifier<'ctx>>>),
    Ptr(Box<TypeSpecifier<'ctx>>),
}

#[derive(Debug)]
pub enum AstNodeType<'ctx> {
    Module(Vec<AstNodeRef<'ctx>>),
    Func {
        name: &'ctx str,
        args: Vec<AstFnArg<'ctx>>,
        ret: Option<TypeSpecifier<'ctx>>,
        body: Option<AstNodeRef<'ctx>>, // None for forward decls
    },
    StaticVar {
        name: &'ctx str,
        is_const: bool,
        typespec: Option<TypeSpecifier<'ctx>>,
        //value: AstNodeRef<'ctx>
    },
    StructDecl {
        name: &'ctx str,
        members: Vec<(&'ctx str, TypeSpecifier<'ctx>)>,
    },
    Asm(&'ctx str),
    ExprList(Vec<AstNodeRef<'ctx>>),
    ExprVoidLiteral,
    ExprLocalScope {
        var_name: &'ctx str,
        var_type: Option<TypeSpecifier<'ctx>>,
        value: Option<AstNodeRef<'ctx>>, // a BinOp::Assign is inserted in scoped_expr, so this is
        // not used except for type inference
        scoped_expr: AstNodeRef<'ctx>,
    },
    ExprIdent(&'ctx str),
    ExprBinop {
        op: BinOp,
        args: [AstNodeRef<'ctx>; 2],
    },
    ExprLoop {
        label: Option<&'ctx str>,
        condition: Option<AstNodeRef<'ctx>>,
        body: AstNodeRef<'ctx>,
        on_next_iter: Option<AstNodeRef<'ctx>>,
    },
    ExprIfElse {
        condition: AstNodeRef<'ctx>,
        on_true: AstNodeRef<'ctx>,
        on_false: AstNodeRef<'ctx>,
    },
}
