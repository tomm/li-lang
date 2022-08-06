use crate::source_resolver::Sources;
use crate::error::CompileError;

#[derive(Debug,PartialEq,Eq)]
pub enum Token {
    Unknown,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LSqBracket,
    RSqBracket,
    Semicolon,
    Colon,
    Minus,
    Plus,
    Hash,
    AtSign,
    Ampersand,
    Exclamation,
    Pipe,
    Tilde,
    Acute,
    Asterisk,
    Slash,
    Percent,
    Dollar,
    Comma,
    Period,
    Assign,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    LShiftAssign,
    //  T_RSHIFTASSIGN,
    //  T_BITANDASSIGN,
    //  T_BITORASSIGN,
    //  T_BITXORASSIGN,
    CmpEq,
    CmpNeq,
    //  T_GT,
    //  T_GTE,
    CmpLt,
    CmpLte,
    LShift,
    //  T_SHIFT_RIGHT,
    //  T_AS,
    //  T_IF,
    //  T_ELSE,
    //  T_RETURN,
    Func,
    Var,
    Const,
    //  T_BREAK,
    //  T_CONTINUE,
    //  T_STRUCT,
    //  T_IDENT,
    //  T_JUMP_LABEL,
    //  T_LITERAL_STR,
    //  T_LITERAL_U8,
    //  T_LITERAL_U16,
    //  T_LITERAL_I8,
    //  T_LITERAL_I16,
    //  T_LITERAL_ANY_INT,
    //  T_LITERAL_TRUE,
    //  T_LITERAL_FALSE,
    //  T_ASM,
    RArrow,
    //  T_WHILE,
    //  T_LOOP,
    //  T_FOR,
}

#[derive(Debug,PartialEq,Eq)]
pub struct TokenLoc<'a> {
    pub token: Token,
    pub line: i32,
    pub col: i32,
    pub filename: &'a str
}

pub fn lex_file<'ctx, 'a>(sources: &'ctx mut Sources, filename: &'a str) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    // Get 'ctx scoped file contents and filename
    let (_filename, buf) = sources.get_src(filename);
    lex(buf, _filename)
}

pub fn lex<'ctx>(buf: &'ctx str, filename: &'ctx str) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    let mut tokens: Vec<TokenLoc> = Vec::new();

    let mut line = 1;
    let mut col = 1;
    // can't have unseparated literals (like 123"hello")
    let last_tok_was_literal = false;

    let mut i = buf.chars();

    println!("len {}", buf.len());
    'outer: loop {
        macro_rules! peek {
            () => {
                match i.clone().next() {
                    Some(c) => c,
                    None => break 'outer
                }
            };
        }

        macro_rules! skip {
            () => {
                match i.next() {
                    Some(c) => {
                        if c == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                    }
                    None => break 'outer
                }
            };
        }

        macro_rules! lookahead {
            ($n: expr) => {
                i.clone().skip($n).next().unwrap_or('\0')
            };
        }

        macro_rules! emit {
            ($n: expr, $a: expr) => {
                {
                    tokens.push(TokenLoc {
                        line,
                        col,
                        filename,
                        token: $a,
                    });
                    for _ in 0..$n {
                        skip!();
                    }
                }
            };
        }

        match peek!() {
            '{' => emit!(1, Token::LBrace),
            '}' => emit!(1, Token::RBrace),
            '(' => emit!(1, Token::LParen),
            ')' => emit!(1, Token::RParen),
            '[' => emit!(1, Token::LSqBracket),
            ']' => emit!(1, Token::RSqBracket),
            ';' => emit!(1, Token::Semicolon),
            ':' => emit!(1, Token::Colon),
            '~' => emit!(1, Token::Tilde),
            '#' => emit!(1, Token::Hash),
            '@' => emit!(1, Token::AtSign),
            '&' => emit!(1, Token::Ampersand),
            '|' => emit!(1, Token::Pipe),
            '^' => emit!(1, Token::Acute),
            '$' => emit!(1, Token::Dollar),
            ',' => emit!(1, Token::Comma),
            '.' => emit!(1, Token::Period),
            '+' => match lookahead!(1) {
                '=' => emit!(2, Token::PlusAssign),
                _ => emit!(1, Token::Plus),
            },
            '-' => match lookahead!(1) {
                '>' => emit!(2, Token::RArrow),
                '=' => emit!(2, Token::MinusAssign),
                _ => emit!(1, Token::Minus),
            },
            '*' => match lookahead!(1) {
                '=' => emit!(2, Token::MulAssign),
                _ => emit!(1, Token::Asterisk)
            },
            '%' => match lookahead!(1) {
                '=' => emit!(2, Token::ModAssign),
                _ => emit!(1, Token::Percent)
            },
            '=' => match lookahead!(1) {
                '=' => emit!(2, Token::CmpEq),
                _ => emit!(1, Token::Assign)
            },
            '<' => match lookahead!(1) {
                '<' => match lookahead!(2) {
                    '=' => emit!(3, Token::LShiftAssign),
                    _ => emit!(2, Token::LShift)
                },
                '=' => emit!(2, Token::CmpLte),
                _ => emit!(1, Token::CmpLt)
            },
            '/' => match lookahead!(1) {
                '/' => {
                    // C++-style comment
                    loop {
                        skip!();
                        if peek!() == '\n' { break }
                    }
                }
                '*' => {
                    // C-style comment
                    loop {
                        skip!();
                        if peek!() == '*' && lookahead!(1) == '/' { skip!(); skip!(); break }
                    }
                },
                '=' => emit!(2, Token::DivAssign),
                _ => emit!(1, Token::Slash)
            },
            '!' => match lookahead!(1) {
                '=' => emit!(2, Token::CmpNeq),
                _ => emit!(1, Token::Exclamation)
            },
            ' ' | '\t' | '\n' | '\r' => {
                // ignore
                skip!();
            }
            other => {
                if !last_tok_was_literal && (other == '_' || other.is_alphabetic()) {
                    let tok_end = i.as_str().find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(i.as_str().len());
                    let tok = i.as_str().get(0..tok_end).unwrap();

                    match tok {
                        "fn" => emit!(2, Token::Func),
                        "var" => emit!(3, Token::Var),
                        "const" => emit!(5, Token::Const),
                        other => {
                            return Err(
                                CompileError {
                                    loc: TokenLoc {
                                        token: Token::Unknown,
                                        line,
                                        col,
                                        filename,
                                    },
                                    msg: format!("Unexpected token '{}'", other)
                                }
                            );
                        }
                    }

                    /*
                    let tok = i.as_str().get(0..(i.as_str().find(|c| c.is_alphanumeric() || c == '_').unwrap()));
                    loop {
                        let ch = tok_end.next().unwrap_or('\0');
                        if !ch.is_alphabetic() && ch != '_' {
                            break;
                        }
                    }
                    let tok = std::str::to_slice(i, tok_end);
                    */
                } else {
                    return Err(
                        CompileError {
                            loc: TokenLoc {
                                token: Token::Unknown,
                                line,
                                col,
                                filename,
                            },
                            msg: format!("Unexpected token '{}'", other)
                        }
                    );
                }
            }
        };
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn all_tokens() {
        let filename = "test.li";
        assert_eq!(
            lex("{}->-=-()[];:~+#@&|^fn var const*=*!!=/=/ // commented out\n%%=/* C-style comment */$,.===++= < <= << <<=", filename),
            Ok(vec![
                TokenLoc { token: Token::LBrace, line: 1, col: 1, filename },
                TokenLoc { token: Token::RBrace, line: 1, col: 2, filename },
                TokenLoc { token: Token::RArrow, line: 1, col: 3, filename },
                TokenLoc { token: Token::MinusAssign, line: 1, col: 5, filename },
                TokenLoc { token: Token::Minus, line: 1, col: 7, filename },
                TokenLoc { token: Token::LParen, line: 1, col: 8, filename },
                TokenLoc { token: Token::RParen, line: 1, col: 9, filename },
                TokenLoc { token: Token::LSqBracket, line: 1, col: 10, filename },
                TokenLoc { token: Token::RSqBracket, line: 1, col: 11, filename },
                TokenLoc { token: Token::Semicolon, line: 1, col: 12, filename },
                TokenLoc { token: Token::Colon, line: 1, col: 13, filename },
                TokenLoc { token: Token::Tilde, line: 1, col: 14, filename },
                TokenLoc { token: Token::Plus, line: 1, col: 15, filename },
                TokenLoc { token: Token::Hash, line: 1, col: 16, filename },
                TokenLoc { token: Token::AtSign, line: 1, col: 17, filename },
                TokenLoc { token: Token::Ampersand, line: 1, col: 18, filename },
                TokenLoc { token: Token::Pipe, line: 1, col: 19, filename },
                TokenLoc { token: Token::Acute, line: 1, col: 20, filename },
                TokenLoc { token: Token::Func, line: 1, col: 21, filename },
                TokenLoc { token: Token::Var, line: 1, col: 24, filename },
                TokenLoc { token: Token::Const, line: 1, col: 28, filename },
                TokenLoc { token: Token::MulAssign, line: 1, col: 33, filename },
                TokenLoc { token: Token::Asterisk, line: 1, col: 35, filename },
                TokenLoc { token: Token::Exclamation, line: 1, col: 36, filename },
                TokenLoc { token: Token::CmpNeq, line: 1, col: 37, filename },
                TokenLoc { token: Token::DivAssign, line: 1, col: 39, filename },
                TokenLoc { token: Token::Slash, line: 1, col: 41, filename },
                TokenLoc { token: Token::Percent, line: 2, col: 1, filename },
                TokenLoc { token: Token::ModAssign, line: 2, col: 2, filename },
                TokenLoc { token: Token::Dollar, line: 2, col: 25, filename },
                TokenLoc { token: Token::Comma, line: 2, col: 26, filename },
                TokenLoc { token: Token::Period, line: 2, col: 27, filename },
                TokenLoc { token: Token::CmpEq, line: 2, col: 28, filename },
                TokenLoc { token: Token::Assign, line: 2, col: 30, filename },
                TokenLoc { token: Token::Plus, line: 2, col: 31, filename },
                TokenLoc { token: Token::PlusAssign, line: 2, col: 32, filename },
                TokenLoc { token: Token::CmpLt, line: 2, col: 35, filename },
                TokenLoc { token: Token::CmpLte, line: 2, col: 37, filename },
                TokenLoc { token: Token::LShift, line: 2, col: 40, filename },
                TokenLoc { token: Token::LShiftAssign, line: 2, col: 43, filename },
            ])
        );
    }

    #[test]
    fn whitespace() {
        let filename = "test.li";
        assert_eq!(
            lex("  {\r\n    {}\n  }\n\t{}", filename),
            Ok(vec![
                TokenLoc { token: Token::LBrace, line: 1, col: 3, filename },
                TokenLoc { token: Token::LBrace, line: 2, col: 5, filename },
                TokenLoc { token: Token::RBrace, line: 2, col: 6, filename },
                TokenLoc { token: Token::RBrace, line: 3, col: 3, filename },
                TokenLoc { token: Token::LBrace, line: 4, col: 2, filename },
                TokenLoc { token: Token::RBrace, line: 4, col: 3, filename },
            ])
        );
    }

    #[test]
    fn keyword_separation() {
        let filename = "test.li";
        assert_eq!(
            lex(" fnvar", filename),
            Err(CompileError {
                loc: TokenLoc { token: Token::Unknown, line: 1, col: 2, filename },
                msg: "Unexpected token 'fnvar'".to_string()
            })
        );
    }
}
