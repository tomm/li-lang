use crate::ast::{
    AstFnArg, AstNode, AstNodeMutRef, AstNodeRef, AstNodeType, LiteralIntExprType, Op,
    TypeSpecifier,
};
use crate::error::CompileError;
use crate::lex::{lex_file, LiteralIntTokenType, Token, TokenLoc};
use crate::Ctx;
use std::cell::Cell;

macro_rules! error {
    ($loc: expr, $msg: expr) => {
        return Err(CompileError {
            loc: $loc,
            msg: $msg,
        })
    };
}

type TokIter<'ctx, 'a> = std::iter::Peekable<std::slice::Iter<'a, TokenLoc<'ctx>>>;
type ParseOne<'ctx> = Result<AstNodeRef<'ctx>, CompileError<'ctx>>;
type ParseMany<'ctx> = Result<Vec<AstNodeRef<'ctx>>, CompileError<'ctx>>;

struct Parser<'ctx, 'a> {
    toks: &'a mut TokIter<'ctx, 'a>,
    context: Ctx<'ctx>,
}

/*
 * Lifetimes: 'ctx is compiler Context, 'ts is token stream vec
 */
impl<'ctx, 'ts> Parser<'ctx, 'ts> {
    fn new(context: Ctx<'ctx>, toks: &'ts mut TokIter<'ctx, 'ts>) -> Self {
        Parser { context, toks }
    }

    fn peek(self: &mut Self) -> &TokenLoc<'ctx> {
        self.toks.peek().unwrap()
    }

    fn new_node(
        self: &mut Self,
        loc: TokenLoc<'ctx>,
        node_type: AstNodeType<'ctx>,
    ) -> AstNodeMutRef<'ctx> {
        self.context.alloc_astnode(AstNode::new(loc, node_type))
    }

    #[must_use]
    fn chomp(self: &mut Self, token: Token) -> Result<TokenLoc<'ctx>, CompileError<'ctx>> {
        let t = self.toks.next().unwrap();
        if t.token != token {
            error!(*t, format!("Expected {:?} here", token));
        } else {
            Ok(*t)
        }
    }

    fn maybe_chomp(self: &mut Self, token: Token) -> bool {
        if self.toks.peek().unwrap().token == token {
            self.toks.next();
            true
        } else {
            false
        }
    }

    fn chomp_ident(self: &mut Self) -> Result<&'ctx str, CompileError<'ctx>> {
        match self.toks.next().unwrap() {
            TokenLoc {
                token: Token::Ident(name),
                ..
            } => Ok(name),
            t => error!(*t, "Expected identifier".to_string()),
        }
    }

    fn parse_asm_literal(self: &mut Self) -> ParseOne<'ctx> {
        let t = self.toks.next().unwrap();
        match t.token {
            Token::Asm(asm_text) | Token::LiteralStr(asm_text) => {
                Ok(self.new_node(*t, AstNodeType::Asm(asm_text)))
            }
            _ => panic!(),
        }
    }

    fn parse_module(self: &mut Self) -> ParseOne<'ctx> {
        let mut top_level_nodes: Vec<&AstNode> = vec![];

        let start = *self.peek();

        while let Some(&&tok) = self.toks.peek() {
            match tok.token {
                Token::Const => top_level_nodes.extend(self.parse_static_var_def()?),
                Token::Var => top_level_nodes.extend(self.parse_static_var_def()?),
                Token::Func => top_level_nodes.push(self.parse_function()?),
                Token::Asm(..) => top_level_nodes.push(self.parse_asm_literal()?),
                Token::Ident(..) => {
                    match self.chomp_ident()? {
                        "asm" => {
                            self.chomp(Token::LParen)?;
                            top_level_nodes.push(self.parse_asm_literal()?);
                            self.chomp(Token::RParen)?;
                        }
                        _ => {
                            // XXX TODO "include"
                            error!(tok, "Unexpected identifier".to_string());
                        }
                    }
                }
                Token::Struct => top_level_nodes.push(self.parse_struct_def()?),
                Token::EOF => break,
                _ => error!(tok, format!("Unexpected token {:?}", tok.token)),
            }
        }

        Ok(self.new_node(start, AstNodeType::Module(top_level_nodes)))
    }

    fn parse_function(self: &mut Self) -> ParseOne<'ctx> {
        self.chomp(Token::Func)?;

        let t = self.toks.next().unwrap();

        if let Token::Ident(fn_name) = t.token {
            self.chomp(Token::LParen)?;
            let mut args = vec![];
            while matches!(self.peek().token, Token::Ident(..)) {
                let name = match self.toks.next().unwrap() {
                    TokenLoc {
                        token: Token::Ident(name),
                        ..
                    } => name,
                    tloc => error!(*tloc, "Expected function argument name".to_string()),
                };
                self.chomp(Token::Colon)?;
                let type_ = self.parse_typespecifier()?;
                args.push(AstFnArg { name, type_ });

                if !self.maybe_chomp(Token::Comma) {
                    break;
                }
            }
            self.chomp(Token::RParen)?;

            let ret = if self.maybe_chomp(Token::RArrow) {
                Some(self.parse_typespecifier()?)
            } else {
                None
            };

            let body = if self.maybe_chomp(Token::Semicolon) {
                None
            } else {
                self.chomp(Token::LBrace)?;
                let _body = self.parse_list_expression(Token::RBrace)?;
                self.chomp(Token::RBrace)?;
                Some(_body)
            };

            Ok(self.new_node(
                *t,
                AstNodeType::Func {
                    name: fn_name,
                    args,
                    ret,
                    body,
                },
            ))
        } else {
            error!(*t, "Expected function name".to_string());
        }
    }

    fn parse_static_var_def(self: &mut Self) -> ParseMany<'ctx> {
        let mut defs: Vec<AstNodeRef<'ctx>> = vec![];
        let is_const = match self.toks.next().unwrap().token {
            Token::Var => false,
            Token::Const => true,
            _ => panic!(),
        };

        loop {
            let ident = self.toks.next().unwrap();

            if let Token::Ident(name) = ident.token {
                let typespec = match self.toks.peek().unwrap().token {
                    Token::Colon => {
                        self.toks.next();
                        Some(self.parse_typespecifier()?)
                    }
                    _ => None,
                };

                defs.push(self.new_node(
                    *ident,
                    AstNodeType::StaticVar {
                        name,
                        is_const,
                        typespec,
                    },
                ));
            } else {
                error!(*ident, "Expected variable name".to_string());
            }

            if !self.maybe_chomp(Token::Comma) {
                break;
            }
        }

        Ok(defs)
    }

    fn parse_typespecifier(self: &mut Self) -> Result<TypeSpecifier<'ctx>, CompileError<'ctx>> {
        let t = self.toks.next().unwrap();
        Ok(match t.token {
            Token::Ident(name) => TypeSpecifier::NamedType(name),
            Token::AtSign => TypeSpecifier::Ptr(Box::new(self.parse_typespecifier()?)),
            Token::LSqBracket => {
                let contained = self.parse_typespecifier()?;
                self.chomp(Token::Semicolon)?;
                let size = match self.toks.next().unwrap() {
                    TokenLoc {
                        token: Token::LiteralInt(value, _),
                        ..
                    } => *value as usize,
                    t => error!(*t, "Expected array size".to_string()),
                };
                self.chomp(Token::RSqBracket)?;
                TypeSpecifier::Array(Box::new(contained), size)
            }
            Token::Func => {
                self.chomp(Token::LParen)?;
                let mut args = vec![];
                while self.toks.peek().unwrap().token != Token::RParen {
                    args.push(self.parse_typespecifier()?);
                    if !self.maybe_chomp(Token::Comma) {
                        break;
                    }
                }
                self.chomp(Token::RParen)?;
                let ret = match self.toks.peek().unwrap().token {
                    Token::RArrow => {
                        self.chomp(Token::RArrow)?;
                        Some(Box::new(self.parse_typespecifier()?))
                    }
                    _ => None,
                };
                TypeSpecifier::Func(args, ret)
            }
            _ => error!(*t, "Expected type specifier".to_string()),
        })
    }

    fn parse_struct_def(self: &mut Self) -> ParseOne<'ctx> {
        let start = self.chomp(Token::Struct)?;
        let name = self.chomp_ident()?;
        self.chomp(Token::LBrace)?;
        let mut members = vec![];
        while self.peek().token != Token::RBrace {
            let mem_name = self.chomp_ident()?;
            self.chomp(Token::Colon)?;
            let mem_type = self.parse_typespecifier()?;
            members.push((mem_name, mem_type));
        }
        self.chomp(Token::RBrace)?;

        Ok(self.new_node(start, AstNodeType::StructDecl { name, members }))
    }

    fn parse_list_expression(self: &mut Self, terminator: Token) -> ParseOne<'ctx> {
        let start = *self.peek();
        let mut is_void = false;
        let mut exprs = vec![];

        while self.peek().token != terminator {
            exprs.push(self.parse_localscope_expression(terminator)?);
            is_void = self.maybe_chomp(Token::Semicolon);
        }

        if is_void && exprs.len() > 0 {
            exprs.push(self.new_node(start, AstNodeType::ExprVoidLiteral));
        }

        Ok(self.new_node(start, AstNodeType::ExprList(exprs)))
    }

    /* Parse variable declarations in the form:
     * name: type = initial value, ...
     * Where ': type' and '= initial value' are optional.
     */
    fn parse_var_scopes(
        self: &mut Self,
    ) -> Result<
        Vec<(
            &'ctx str,
            Option<TypeSpecifier<'ctx>>,
            Option<AstNodeRef<'ctx>>,
        )>,
        CompileError<'ctx>,
    > {
        let mut vars = vec![];
        while matches!(self.peek().token, Token::Ident(..)) {
            let name = self.chomp_ident()?;
            let typespec = if self.maybe_chomp(Token::Colon) {
                Some(self.parse_typespecifier()?)
            } else {
                None
            };
            let initval = if self.maybe_chomp(Token::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            vars.push((name, typespec, initval));

            if !self.maybe_chomp(Token::Comma) {
                break;
            }
        }
        Ok(vars)
    }

    fn wrap_expression_in_scopes(
        self: &mut Self,
        start_token: TokenLoc<'ctx>,
        mut expr: AstNodeRef<'ctx>,
        scopes: Vec<(
            &'ctx str,
            Option<TypeSpecifier<'ctx>>,
            Option<AstNodeRef<'ctx>>,
        )>,
    ) -> AstNodeRef<'ctx> {
        for (name, typespec, initval) in scopes.into_iter().rev() {
            if let Some(initval) = initval {
                expr = self.insert_assignment(expr, start_token, name, initval);
            }
            expr = self.new_node(
                start_token,
                AstNodeType::ExprLocalScope {
                    var_name: name,
                    var_type: typespec,
                    value: initval,
                    scoped_expr: expr,
                },
            );
        }

        expr
    }

    fn parse_localscope_expression(self: &mut Self, terminator: Token) -> ParseOne<'ctx> {
        if self.peek().token == Token::Var {
            let start = *self.peek();
            self.chomp(Token::Var)?;

            let var_scopes = self.parse_var_scopes()?;
            let scoped_expr = self.parse_list_expression(terminator)?;

            Ok(self.wrap_expression_in_scopes(start, scoped_expr, var_scopes))
        } else {
            self.parse_expression()
        }
    }

    fn parse_expression(self: &mut Self) -> ParseOne<'ctx> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_conditional_expression()?;
        let start = *self.peek();

        match start.token {
            Token::Assign
            | Token::MulAssign
            | Token::DivAssign
            | Token::ModAssign
            | Token::LShiftAssign
            | Token::RShiftAssign
            | Token::BitAndAssign
            | Token::BitOrAssign
            | Token::BitXorAssign
            | Token::PlusAssign
            | Token::MinusAssign => {
                self.toks.next();
                let arg1 = n;
                let arg2 = self.parse_assignment_expression()?;

                Ok(self.new_node(
                    start,
                    AstNodeType::ExprBinop {
                        op: match start.token {
                            Token::Assign => Op::Assign,
                            Token::MulAssign => Op::MulAssign,
                            Token::DivAssign => Op::DivAssign,
                            Token::ModAssign => Op::ModAssign,
                            Token::LShiftAssign => Op::LShiftAssign,
                            Token::RShiftAssign => Op::RShiftAssign,
                            Token::BitAndAssign => Op::BitAndAssign,
                            Token::BitOrAssign => Op::BitOrAssign,
                            Token::BitXorAssign => Op::BitXorAssign,
                            Token::PlusAssign => Op::PlusAssign,
                            Token::MinusAssign => Op::MinusAssign,
                            _ => panic!(),
                        },
                        args: [arg1, arg2],
                    },
                ))
            }
            _ => Ok(n),
        }
    }

    fn parse_conditional_expression(self: &mut Self) -> ParseOne<'ctx> {
        let start = *self.peek();

        let label = match start {
            TokenLoc {
                token: Token::JumpLabel(target),
                ..
            } => {
                self.toks.next();
                self.chomp(Token::Colon)?;

                let labelled = self.peek();
                if labelled.token != Token::While
                    && labelled.token != Token::Loop
                    && labelled.token != Token::For
                {
                    error!(
                        *labelled,
                        "Expected while/for/loop after jump label".to_string()
                    )
                }
                Some(target)
            }
            _ => None,
        };

        match start.token {
            Token::While => {
                self.toks.next();
                let condition = self.parse_expression()?;
                self.chomp(Token::LBrace)?;
                let body = self.parse_expression()?;
                self.chomp(Token::RBrace)?;

                Ok(self.new_node(
                    start,
                    AstNodeType::ExprLoop {
                        label,
                        condition: Some(condition),
                        body,
                        on_next_iter: None,
                    },
                ))
            }
            Token::Loop => {
                self.toks.next();
                self.chomp(Token::LBrace)?;
                let body = self.parse_expression()?;
                self.chomp(Token::RBrace)?;

                Ok(self.new_node(
                    start,
                    AstNodeType::ExprLoop {
                        label,
                        condition: None,
                        body,
                        on_next_iter: None,
                    },
                ))
            }
            Token::For => {
                self.chomp(Token::For)?;

                let var_scopes = self.parse_var_scopes()?;
                self.chomp(Token::Semicolon)?;

                let condition = match self.peek().token {
                    Token::Semicolon => None,
                    _ => Some(self.parse_expression()?),
                };
                self.chomp(Token::Semicolon)?;

                let on_next_iter = match self.peek().token {
                    Token::Semicolon => None,
                    _ => Some(self.parse_expression()?),
                };

                self.chomp(Token::LBrace)?;
                let body = self.parse_list_expression(Token::RBrace)?;
                self.chomp(Token::RBrace)?;

                let loop_ = self.new_node(
                    start,
                    AstNodeType::ExprLoop {
                        label,
                        condition,
                        body,
                        on_next_iter,
                    },
                );

                Ok(self.wrap_expression_in_scopes(start, loop_, var_scopes))
            }
            Token::If => {
                let start = self.chomp(Token::If)?;
                let condition = self.parse_expression()?;
                let on_true = self.parse_expression()?;
                let on_false = if self.maybe_chomp(Token::Else) {
                    self.parse_expression()?
                } else {
                    self.new_node(start, AstNodeType::ExprVoidLiteral)
                };

                Ok(self.new_node(
                    start,
                    AstNodeType::ExprIfElse {
                        condition,
                        on_true,
                        on_false,
                    },
                ))
            }
            _ => self.parse_comparison_expression(),
        }
    }

    fn parse_comparison_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_logical_expression()?;
        Ok(n)
    }

    fn parse_logical_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_additive_expression()?;
        Ok(n)
    }

    fn parse_additive_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_multiplicative_expression()?;
        Ok(n)
    }

    fn parse_multiplicative_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_unary_expression()?;
        Ok(n)
    }

    fn parse_unary_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_postfix_expression()?;
        Ok(n)
    }

    fn parse_postfix_expression(self: &mut Self) -> ParseOne<'ctx> {
        let n = self.parse_primary_expression()?;
        Ok(n)
    }

    fn parse_primary_expression(self: &mut Self) -> ParseOne<'ctx> {
        let t = self.toks.next().unwrap();

        match t.token {
            Token::LiteralBool(v) => Ok(self.new_node(*t, AstNodeType::ExprBoolLiteral(v))),
            Token::LiteralInt(v, int_type) => Ok(self.new_node(
                *t,
                AstNodeType::ExprIntLiteral(
                    v,
                    LiteralIntExprType::from_literal_int_token_type(int_type),
                ),
            )),
            Token::LiteralStr(s) => Ok(self.new_node(*t, AstNodeType::ExprStrLiteral(s))),
            Token::Ident(name) => Ok(self.new_node(*t, AstNodeType::ExprIdent(name))),
            Token::LSqBracket => {
                let mut contents = vec![];
                while self.peek().token != Token::RSqBracket {
                    contents.push(self.parse_expression()?);
                    if !self.maybe_chomp(Token::Comma) {
                        break;
                    }
                }
                self.chomp(Token::RSqBracket)?;
                Ok(self.new_node(*t, AstNodeType::ExprArrayLiteral(contents)))
            }
            Token::LBrace => {
                let n = self.parse_list_expression(Token::RBrace)?;
                self.chomp(Token::RBrace)?;
                Ok(n)
            }
            Token::Asm(s) => Ok(self.new_node(*t, AstNodeType::Asm(s))),
            Token::Break | Token::Continue => {
                let label = match self.peek().token {
                    Token::JumpLabel(l) => {
                        self.toks.next();
                        Some(l)
                    }
                    _ => None,
                };
                Ok(self.new_node(
                    *t,
                    AstNodeType::ExprGoto {
                        is_continue: t.token == Token::Continue,
                        label,
                        target: Cell::new(None),
                    },
                ))
            }
            Token::Return => {
                let val = self.parse_expression()?;
                Ok(self.new_node(*t, AstNodeType::ExprReturn(val)))
            }
            tok => error!(
                *t,
                format!("Expected primary expression but found {:?}", tok)
            ),
        }
    }

    /*
     * Insert an assignment operator at the beginning of `scoped_expr`,
     * assigning `ident` = `value`
     * Returns AstNodeRef of modified scoped_expr
     */
    fn insert_assignment(
        self: &mut Self,
        scoped_expr: AstNodeRef<'ctx>,
        start: TokenLoc<'ctx>,
        var_name: &'ctx str,
        value: AstNodeRef<'ctx>,
    ) -> AstNodeRef<'ctx> {
        let ident_expr = self.new_node(start, AstNodeType::ExprIdent(var_name));

        let assignment = self.new_node(
            start,
            AstNodeType::ExprBinop {
                op: Op::Assign,
                args: [ident_expr, value],
            },
        );

        self.new_node(start, AstNodeType::ExprList(vec![assignment, scoped_expr]))
    }
}

pub fn parse_file<'ctx>(context: Ctx<'ctx>, filename: &str) -> Result<(), CompileError<'ctx>> {
    println!("Hello from the parser: {}", filename);

    let tokens = lex_file(context, filename)?;

    let mut iter = tokens.iter().peekable();
    let module = Parser::new(context, &mut iter).parse_module()?;
    module.pretty_print(0);
    Ok(())
}
