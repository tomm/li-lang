use crate::error::CompileError;
use crate::Ctx;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LiteralIntTokenType {
    Unknown,
    U8,
    I8,
    U16,
    I16,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    EOF,
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
    RShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    CmpEq,
    CmpNeq,
    CmpGt,
    CmpGte,
    CmpLt,
    CmpLte,
    LShift,
    RShift,
    As,
    If,
    Else,
    Return,
    Func,
    Var,
    Const,
    Break,
    Continue,
    Struct,
    Ident(&'a str),
    JumpLabel(&'a str),
    LiteralStr(&'a str),
    LiteralInt(i32, LiteralIntTokenType),
    LiteralBool(bool),
    Asm(&'a str),
    RArrow,
    While,
    Loop,
    For,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenLoc<'a> {
    pub token: Token<'a>,
    pub line: i32,
    pub col: i32,
    pub filename: &'a str,
}

pub fn lex_file<'ctx, 'a>(
    context: Ctx<'ctx>,
    filename: &'a str,
) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    // Get 'ctx scoped file contents and filename
    let (_filename, buf) = context.sources.get_src(filename);
    lex(buf, _filename)
}

#[derive(Clone)]
struct LocStrIter<'a> {
    pub line: i32,
    pub col: i32,
    i: std::str::Chars<'a>,
}

impl<'a> LocStrIter<'a> {
    fn from_str(s: &'a str) -> Self {
        LocStrIter {
            line: 1,
            col: 1,
            i: s.chars(),
        }
    }
    fn as_str(&self) -> &'a str {
        self.i.as_str()
    }
    fn read_identifier(&mut self) -> Option<&'a str> {
        let tok_end = self
            .i
            .as_str()
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(self.i.as_str().len());
        if tok_end == 0 {
            None
        } else {
            let tok = self.i.as_str().get(0..tok_end).unwrap();
            self.nth(tok_end - 1);
            Some(tok)
        }
    }
}

impl<'a> Iterator for LocStrIter<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        match self.i.next() {
            Some(c) => {
                if c == '\n' {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                Some(c)
            }
            None => None,
        }
    }
}

pub fn lex<'ctx>(
    buf: &'ctx str,
    filename: &'ctx str,
) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    let mut tokens: Vec<TokenLoc> = Vec::new();

    // can't have unseparated literals (like 123"hello")
    let mut last_tok_needs_space = false;
    let mut tok_line: i32;
    let mut tok_col: i32;

    let mut i = LocStrIter::from_str(buf);

    'outer: loop {
        macro_rules! next {
            () => {
                i.next().unwrap_or('\0')
            };
        }

        macro_rules! expect {
            ($c: expr) => {
                if next!() != $c {
                    error!(format!("Expected '{}'", $c));
                }
            };
        }

        macro_rules! peek {
            ($n: expr) => {
                i.clone().skip($n).next().unwrap_or('\0')
            };
        }

        macro_rules! token {
            ($n: expr, $a: expr) => {{
                tokens.push(TokenLoc {
                    line: tok_line,
                    col: tok_col,
                    filename,
                    token: $a,
                });
                for _ in 0..$n {
                    next!();
                }
                last_tok_needs_space = false;
            }};
        }

        /* Emit a token that needs to be separated from other anti-social tokens
         * (by whitespace or non-antisocial tokens) */
        macro_rules! antisocial_token {
            ($n: expr, $a: expr) => {{
                tokens.push(TokenLoc {
                    line: tok_line,
                    col: tok_col,
                    filename,
                    token: $a,
                });
                for _ in 0..$n {
                    next!();
                }
                last_tok_needs_space = true;
            }};
        }

        macro_rules! error {
            ($msg: expr) => {
                return Err(CompileError {
                    loc: TokenLoc {
                        token: Token::Unknown,
                        line: tok_line,
                        col: tok_col,
                        filename,
                    },
                    msg: $msg,
                })
            };
        }

        macro_rules! mark_token_start {
            () => {
                tok_line = i.line;
                tok_col = i.col;
            };
        }

        mark_token_start!();

        match peek!(0) {
            '\0' => break 'outer,
            '{' => token!(1, Token::LBrace),
            '}' => token!(1, Token::RBrace),
            '(' => token!(1, Token::LParen),
            ')' => token!(1, Token::RParen),
            '[' => token!(1, Token::LSqBracket),
            ']' => token!(1, Token::RSqBracket),
            ';' => token!(1, Token::Semicolon),
            ':' => token!(1, Token::Colon),
            '~' => token!(1, Token::Tilde),
            '#' => token!(1, Token::Hash),
            '@' => token!(1, Token::AtSign),
            '&' => match peek!(1) {
                '=' => token!(2, Token::BitAndAssign),
                _ => token!(1, Token::Ampersand),
            },
            '|' => match peek!(1) {
                '=' => token!(2, Token::BitOrAssign),
                _ => token!(1, Token::Pipe),
            },
            '^' => match peek!(1) {
                '=' => token!(2, Token::BitXorAssign),
                _ => token!(1, Token::Acute),
            },
            '$' => token!(1, Token::Dollar),
            ',' => token!(1, Token::Comma),
            '.' => token!(1, Token::Period),
            '+' => match peek!(1) {
                '=' => token!(2, Token::PlusAssign),
                _ => token!(1, Token::Plus),
            },
            '-' => match peek!(1) {
                '>' => token!(2, Token::RArrow),
                '=' => token!(2, Token::MinusAssign),
                _ => token!(1, Token::Minus),
            },
            '*' => match peek!(1) {
                '=' => token!(2, Token::MulAssign),
                _ => token!(1, Token::Asterisk),
            },
            '%' => match peek!(1) {
                '=' => token!(2, Token::ModAssign),
                _ => token!(1, Token::Percent),
            },
            '=' => match peek!(1) {
                '=' => token!(2, Token::CmpEq),
                _ => token!(1, Token::Assign),
            },
            '<' => match peek!(1) {
                '<' => match peek!(2) {
                    '=' => token!(3, Token::LShiftAssign),
                    _ => token!(2, Token::LShift),
                },
                '=' => token!(2, Token::CmpLte),
                _ => token!(1, Token::CmpLt),
            },
            '>' => match peek!(1) {
                '>' => match peek!(2) {
                    '=' => token!(3, Token::RShiftAssign),
                    _ => token!(2, Token::RShift),
                },
                '=' => token!(2, Token::CmpGte),
                _ => token!(1, Token::CmpGt),
            },
            '/' => match peek!(1) {
                '/' => {
                    // C++-style comment
                    loop {
                        next!();
                        if peek!(0) == '\n' || peek!(0) == '\0' {
                            break;
                        }
                    }
                }
                '*' => {
                    // C-style comment
                    loop {
                        next!();
                        if peek!(0) == '\0' {
                            error!("Unterminated comment. Expected: */".to_string());
                        }
                        if peek!(0) == '*' && peek!(1) == '/' {
                            next!();
                            next!();
                            break;
                        }
                    }
                }
                '=' => token!(2, Token::DivAssign),
                _ => token!(1, Token::Slash),
            },
            '!' => match peek!(1) {
                '=' => token!(2, Token::CmpNeq),
                _ => token!(1, Token::Exclamation),
            },
            ' ' | '\t' | '\n' | '\r' => {
                // ignore
                next!();
                last_tok_needs_space = false;
            }
            '\'' => {
                if peek!(2) != '\'' && (peek!(1).is_alphabetic() || peek!(1) == '_') {
                    expect!('\'');
                    let tok_end = i
                        .as_str()
                        .find(|c: char| !c.is_alphanumeric() && c != '_')
                        .unwrap_or(i.as_str().len());
                    let tok = i.as_str().get(0..tok_end).unwrap();

                    antisocial_token!(tok.len(), Token::JumpLabel(tok));
                } else if peek!(1).is_ascii() && peek!(2) == '\'' {
                    antisocial_token!(
                        3,
                        Token::LiteralInt(peek!(1) as i32, LiteralIntTokenType::U8)
                    );
                } else {
                    error!("Unexpected '".to_string());
                }
            }
            '`' => {
                next!(); // starting `
                if let Some(len) = i.as_str().find(|c: char| c == '`') {
                    let asm = i.as_str().get(0..len).unwrap();
                    antisocial_token!(asm.len(), Token::Asm(asm));
                    next!(); // terminating `
                } else {
                    error!("Unterminated asm literal".to_string());
                }
            }
            '"' => {
                if last_tok_needs_space {
                    error!("Unexpected token '\"'".to_string());
                } else {
                    next!(); // terminating "
                    if let Some(len) = i.as_str().find(|c: char| c == '"') {
                        let tok = i.as_str().get(0..len).unwrap();
                        antisocial_token!(tok.len(), Token::LiteralStr(tok));
                        next!(); // terminating "
                    } else {
                        error!("Unterminated string literal".to_string());
                    }
                }
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut val: i32 = 0;

                macro_rules! read_int {
                    ($radix: expr) => {
                        loop {
                            if peek!(0) == '_' {
                                next!();
                                continue;
                            }
                            match peek!(0).to_digit($radix) {
                                Some(n) => val = val * $radix + n as i32,
                                None => break,
                            }
                            next!();
                        }
                    };
                }

                if peek!(0) == '0' && peek!(1) == 'x' {
                    /* Hex */
                    expect!('0');
                    expect!('x');
                    read_int!(16);
                } else if peek!(0) == '0' && peek!(1) == 'b' {
                    /* Bin */
                    expect!('0');
                    expect!('b');
                    read_int!(2);
                } else {
                    /* Dec */
                    read_int!(10);
                }
                let int_type = match i.read_identifier() {
                    None => LiteralIntTokenType::Unknown,
                    Some("u8") => LiteralIntTokenType::U8,
                    Some("i8") => LiteralIntTokenType::I8,
                    Some("u16") => LiteralIntTokenType::U16,
                    Some("i16") => LiteralIntTokenType::I16,
                    Some(suffix) => error!(format!("Invalid integer suffix: '{}'", suffix)),
                };
                antisocial_token!(0, Token::LiteralInt(val, int_type));
            }
            other => {
                if !last_tok_needs_space && (other == '_' || other.is_alphabetic()) {
                    let tok = i.read_identifier().unwrap();

                    match tok {
                        "fn" => antisocial_token!(0, Token::Func),
                        "var" => antisocial_token!(0, Token::Var),
                        "const" => antisocial_token!(0, Token::Const),
                        "break" => antisocial_token!(0, Token::Break),
                        "continue" => antisocial_token!(0, Token::Continue),
                        "struct" => antisocial_token!(0, Token::Struct),
                        "while" => antisocial_token!(0, Token::While),
                        "loop" => antisocial_token!(0, Token::Loop),
                        "for" => antisocial_token!(0, Token::For),
                        "as" => antisocial_token!(0, Token::As),
                        "if" => antisocial_token!(0, Token::If),
                        "else" => antisocial_token!(0, Token::Else),
                        "return" => antisocial_token!(0, Token::Return),
                        "true" => antisocial_token!(0, Token::LiteralBool(true)),
                        "false" => antisocial_token!(0, Token::LiteralBool(false)),
                        other => antisocial_token!(0, Token::Ident(other)),
                    }
                } else {
                    error!(format!("Unexpected token '{}'", other));
                }
            }
        };
    }

    tokens.push(TokenLoc {
        line: tok_line,
        col: tok_col,
        filename,
        token: Token::EOF,
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn all_tokens() {
        let filename = "test.li";
        assert_eq!(
            lex("{}->-=-()[];:~+#@&|^fn var const*=*!!=/=/ // commented out\n%%=/* C-style comment */$,.===++= < <= << <<=\n\
                break continue struct while loop for as if else return my_var1 _my_var2\n\
                &=|=^= > >= >> >>= `xor b\n\
                and c` \"hello\n\
                world\" true false 'my_label '_other 'x'\n\
                12_3456 0xcAfe 0b101010 0xbabau16 54i8 0u8 0xffe_i16", filename),
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
                TokenLoc { token: Token::Break, line: 3, col: 1, filename },
                TokenLoc { token: Token::Continue, line: 3, col: 7, filename },
                TokenLoc { token: Token::Struct, line: 3, col: 16, filename },
                TokenLoc { token: Token::While, line: 3, col: 23, filename },
                TokenLoc { token: Token::Loop, line: 3, col: 29, filename },
                TokenLoc { token: Token::For, line: 3, col: 34, filename },
                TokenLoc { token: Token::As, line: 3, col: 38, filename },
                TokenLoc { token: Token::If, line: 3, col: 41, filename },
                TokenLoc { token: Token::Else, line: 3, col: 44, filename },
                TokenLoc { token: Token::Return, line: 3, col: 49, filename },
                TokenLoc { token: Token::Ident(&"my_var1"), line: 3, col: 56, filename },
                TokenLoc { token: Token::Ident(&"_my_var2"), line: 3, col: 64, filename },
                TokenLoc { token: Token::BitAndAssign, line: 4, col: 1, filename },
                TokenLoc { token: Token::BitOrAssign, line: 4, col: 3, filename },
                TokenLoc { token: Token::BitXorAssign, line: 4, col: 5, filename },
                TokenLoc { token: Token::CmpGt, line: 4, col: 8, filename },
                TokenLoc { token: Token::CmpGte, line: 4, col: 10, filename },
                TokenLoc { token: Token::RShift, line: 4, col: 13, filename },
                TokenLoc { token: Token::RShiftAssign, line: 4, col: 16, filename },
                TokenLoc { token: Token::Asm("xor b\nand c"), line: 4, col: 20, filename },
                TokenLoc { token: Token::LiteralStr("hello\nworld"), line: 5, col: 8, filename },
                TokenLoc { token: Token::LiteralBool(true), line: 6, col: 8, filename },
                TokenLoc { token: Token::LiteralBool(false), line: 6, col: 13, filename },
                TokenLoc { token: Token::JumpLabel(&"my_label"), line: 6, col: 19, filename },
                TokenLoc { token: Token::JumpLabel(&"_other"), line: 6, col: 29, filename },
                TokenLoc { token: Token::LiteralInt(120, LiteralIntTokenType::U8), line: 6, col: 37, filename },
                TokenLoc { token: Token::LiteralInt(123456, LiteralIntTokenType::Unknown), line: 7, col: 1, filename },
                TokenLoc { token: Token::LiteralInt(0xcafe, LiteralIntTokenType::Unknown), line: 7, col: 9, filename },
                TokenLoc { token: Token::LiteralInt(0b101010, LiteralIntTokenType::Unknown), line: 7, col: 16, filename },
                TokenLoc { token: Token::LiteralInt(0xbaba, LiteralIntTokenType::U16), line: 7, col: 25, filename },
                TokenLoc { token: Token::LiteralInt(54, LiteralIntTokenType::I8), line: 7, col: 35, filename },
                TokenLoc { token: Token::LiteralInt(0, LiteralIntTokenType::U8), line: 7, col: 40, filename },
                TokenLoc { token: Token::LiteralInt(0xffe, LiteralIntTokenType::I16), line: 7, col: 44, filename },
                TokenLoc { token: Token::EOF, line: 7, col: 53, filename },
            ])
        );
    }

    #[test]
    fn whitespace() {
        let filename = "test.li";
        assert_eq!(
            lex("  {\r\n    {}\n  }\n\t{}", filename),
            Ok(vec![
                TokenLoc {
                    token: Token::LBrace,
                    line: 1,
                    col: 3,
                    filename
                },
                TokenLoc {
                    token: Token::LBrace,
                    line: 2,
                    col: 5,
                    filename
                },
                TokenLoc {
                    token: Token::RBrace,
                    line: 2,
                    col: 6,
                    filename
                },
                TokenLoc {
                    token: Token::RBrace,
                    line: 3,
                    col: 3,
                    filename
                },
                TokenLoc {
                    token: Token::LBrace,
                    line: 4,
                    col: 2,
                    filename
                },
                TokenLoc {
                    token: Token::RBrace,
                    line: 4,
                    col: 3,
                    filename
                },
                TokenLoc {
                    token: Token::EOF,
                    line: 4,
                    col: 4,
                    filename
                },
            ])
        );
    }

    #[test]
    fn bad_asm() {
        let filename = "test.li";
        assert_eq!(
            lex(" `ld blah  ", filename),
            Err(CompileError {
                loc: TokenLoc {
                    token: Token::Unknown,
                    line: 1,
                    col: 2,
                    filename
                },
                msg: "Unterminated asm literal".to_string()
            })
        );
    }

    #[test]
    fn bad_int_suffix() {
        let filename = "test.li";
        assert_eq!(
            lex("10u42", filename),
            Err(CompileError {
                loc: TokenLoc {
                    token: Token::Unknown,
                    line: 1,
                    col: 1,
                    filename
                },
                msg: "Invalid integer suffix: 'u42'".to_string()
            })
        );
    }

    #[test]
    fn unterminated_comment() {
        let filename = "test.li";
        assert_eq!(
            lex("// hey", filename),
            Ok(vec![TokenLoc {
                token: Token::EOF,
                line: 1,
                col: 7,
                filename
            },])
        );
        assert_eq!(
            lex("/* hey", filename),
            Err(CompileError {
                loc: TokenLoc {
                    token: Token::Unknown,
                    line: 1,
                    col: 1,
                    filename
                },
                msg: "Unterminated comment. Expected: */".to_string()
            })
        );
    }
}
