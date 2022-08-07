use crate::source_resolver::Sources;
use crate::error::CompileError;

#[derive(Debug,PartialEq,Eq)]
pub enum LiteralIntType {
    Unknown,
    U8,
    I8,
    U16,
    I16,
}

#[derive(Debug,PartialEq,Eq)]
pub enum Token<'a> {
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
    LiteralInt(i32, LiteralIntType),
    LiteralBool(bool),
    Asm(&'a str),
    RArrow,
    While,
    Loop,
    For
}

#[derive(Debug,PartialEq,Eq)]
pub struct TokenLoc<'a> {
    pub token: Token<'a>,
    pub line: i32,
    pub col: i32,
    pub filename: &'a str
}

pub fn lex_file<'ctx, 'a>(sources: &'ctx mut Sources, filename: &'a str) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    // Get 'ctx scoped file contents and filename
    let (_filename, buf) = sources.get_src(filename);
    lex(buf, _filename)
}

#[derive(Clone)]
struct LocStrIter<'a> {
    pub line: i32,
    pub col: i32,
    i: std::str::Chars<'a>
}

impl<'a> LocStrIter<'a> {
    fn from_str(s: &'a str) -> Self {
        LocStrIter { line: 1, col: 1, i: s.chars() }
    }
    fn as_str(&self) -> &'a str {
        self.i.as_str()
    }
    fn read_identifier(&mut self) -> Option<&'a str> {
        let tok_end = self.i.as_str()
                       .find(|c: char| !c.is_alphanumeric() && c != '_')
                       .unwrap_or(self.i.as_str().len());
        if tok_end == 0 {
            None
        } else {
            let tok = self.i.as_str().get(0..tok_end).unwrap();
            self.nth(tok_end-1);
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
            None => None
        }
    }
}

pub fn lex<'ctx>(buf: &'ctx str, filename: &'ctx str) -> Result<Vec<TokenLoc<'ctx>>, CompileError<'ctx>> {
    let mut tokens: Vec<TokenLoc> = Vec::new();

    // can't have unseparated literals (like 123"hello")
    let mut last_tok_needs_space = false;
    let mut tok_line: i32;
    let mut tok_col: i32;

    let mut i = LocStrIter::from_str(buf);

    println!("len {}", buf.len());
    'outer: loop {
        macro_rules! peek {
            () => {
                match i.clone().next() {
                    Some(c) => c,
                    None => '\0'
                }
            };
        }

        macro_rules! skip {
            () => {
                if let Some(c) = i.next() {
                    c
                } else {
                    break 'outer;
                }
            };
        }

        macro_rules! expect {
            ($c: expr) => {
                if skip!() != $c {
                    error!(format!("Expected '{}'", $c));
                }
            };
        }

        macro_rules! lookahead {
            ($n: expr) => {
                i.clone().skip($n).next().unwrap_or('\0')
            };
        }

        /*
        let mut emit = |n: usize, token: Token<'ctx>| {
                    tokens.push(TokenLoc {
                        line: i.line,
                        col: i.col,
                        filename,
                        token
                    });
                    i.nth(n);
                    last_tok_needs_space = false;
        };
        */
        macro_rules! emit {
            ($n: expr, $a: expr) => {
                {
                    tokens.push(TokenLoc {
                        line: tok_line,
                        col: tok_col,
                        filename,
                        token: $a,
                    });
                    for _ in 0..$n {
                        skip!();
                    }
                    last_tok_needs_space = false;
                }
            };
        }

        /* Emit a token that needs to be separated from other anti-social tokens
         * (by whitespace or non-antisocial tokens) */
        macro_rules! emit_antisocial {
            ($n: expr, $a: expr) => {
                {
                    tokens.push(TokenLoc {
                        line: tok_line,
                        col: tok_col,
                        filename,
                        token: $a,
                    });
                    for _ in 0..$n {
                        skip!();
                    }
                    last_tok_needs_space = true;
                }
            };
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
                    msg: $msg
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

        match peek!() {
            '\0' => break 'outer,
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
            '&' => match lookahead!(1) {
                '=' => emit!(2, Token::BitAndAssign),
                _ => emit!(1, Token::Ampersand),
            },
            '|' => match lookahead!(1) {
                '=' => emit!(2, Token::BitOrAssign),
                _ => emit!(1, Token::Pipe),
            },
            '^' => match lookahead!(1) {
                '=' => emit!(2, Token::BitXorAssign),
                _ => emit!(1, Token::Acute),
            },
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
            '>' => match lookahead!(1) {
                '>' => match lookahead!(2) {
                    '=' => emit!(3, Token::RShiftAssign),
                    _ => emit!(2, Token::RShift)
                },
                '=' => emit!(2, Token::CmpGte),
                _ => emit!(1, Token::CmpGt)
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
                last_tok_needs_space = false;
            },
            '\'' => {
                if lookahead!(2) != '\'' && (lookahead!(1).is_alphabetic() || lookahead!(1) == '_') {
                    expect!('\'');
                    let tok_end = i.as_str()
                                   .find(|c: char| !c.is_alphanumeric() && c != '_')
                                   .unwrap_or(i.as_str().len());
                    let tok = i.as_str().get(0..tok_end).unwrap();

                    emit_antisocial!(tok.len(), Token::JumpLabel(tok));
                } else if lookahead!(1).is_ascii() && lookahead!(2) == '\'' {
                    emit_antisocial!(3, Token::LiteralInt(lookahead!(1) as i32, LiteralIntType::U8));
                } else {
                    error!("Unexpected '".to_string());
                }
            },
            '`' => {
                skip!(); // starting `
                if let Some(len) = i.as_str().find(|c: char| c == '`') {
                    let asm = i.as_str().get(0..len).unwrap();
                    emit_antisocial!(asm.len(), Token::Asm(asm));
                    skip!(); // terminating `
                } else {
                    error!("Unterminated asm literal".to_string());
                }
            },
            '"' => {
                if last_tok_needs_space {
                    error!("Unexpected token '\"'".to_string());
                } else {
                    skip!(); // terminating "
                    if let Some(len) = i.as_str().find(|c: char| c == '"') {
                        let tok = i.as_str().get(0..len).unwrap();
                        emit_antisocial!(tok.len(), Token::LiteralStr(tok));
                        skip!(); // terminating "
                    } else {
                        error!("Unterminated string literal".to_string());
                    }
                }
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut val: i32 = 0;

                if peek!() == '0' && lookahead!(1) == 'x' {
                    /* Hex */
                    skip!();
                    skip!();
                    loop {
                        match peek!().to_digit(16) {
                            Some(n) => val = val * 16 + n as i32,
                            None => break
                        }
                        skip!();
                    }
                } else if peek!() == '0' && lookahead!(1) == 'b' {
                    /* Bin */
                    skip!();
                    skip!();
                    loop {
                        match peek!().to_digit(2) {
                            Some(n) => val = val * 2 + n as i32,
                            None => break
                        }
                        skip!();
                    }
                } else {
                    /* Dec */
                    loop {
                        match peek!().to_digit(10) {
                            Some(n) => val = val * 10 + n as i32,
                            None => break
                        }
                        skip!();
                    }
                }
                let int_type = match i.read_identifier() {
                    None => LiteralIntType::Unknown,
                    Some("u8") => LiteralIntType::U8,
                    Some("i8") => LiteralIntType::I8,
                    Some("u16") => LiteralIntType::U16,
                    Some("i16") => LiteralIntType::I16,
                    Some(suffix) => error!(format!("Invalid integer suffix: '{}'", suffix))
                };
                emit_antisocial!(0, Token::LiteralInt(val, int_type));
            },
            other => {
                if !last_tok_needs_space && (other == '_' || other.is_alphabetic()) {
                    let tok = i.read_identifier().unwrap();

                    match tok {
                        "fn" => emit_antisocial!(0, Token::Func),
                        "var" => emit_antisocial!(0, Token::Var),
                        "const" => emit_antisocial!(0, Token::Const),
                        "break" => emit_antisocial!(0, Token::Break),
                        "continue" => emit_antisocial!(0, Token::Continue),
                        "struct" => emit_antisocial!(0, Token::Struct),
                        "while" => emit_antisocial!(0, Token::While),
                        "loop" => emit_antisocial!(0, Token::Loop),
                        "for" => emit_antisocial!(0, Token::For),
                        "as" => emit_antisocial!(0, Token::As),
                        "if" => emit_antisocial!(0, Token::If),
                        "else" => emit_antisocial!(0, Token::Else),
                        "return" => emit_antisocial!(0, Token::Return),
                        "true" => emit_antisocial!(0, Token::LiteralBool(true)),
                        "false" => emit_antisocial!(0, Token::LiteralBool(false)),
                        other => emit_antisocial!(0, Token::Ident(other)),
                    }

                } else {
                    error!(format!("Unexpected token '{}'", other));
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
            lex("{}->-=-()[];:~+#@&|^fn var const*=*!!=/=/ // commented out\n%%=/* C-style comment */$,.===++= < <= << <<=\n\
                break continue struct while loop for as if else return my_var1 _my_var2\n\
                &=|=^= > >= >> >>= `xor b\n\
                and c` \"hello\n\
                world\" true false 'my_label '_other 'x'\n\
                123456 0xcAfe 0b101010 0xbabau16 54i8 0u8 0xffei16", filename),
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
                TokenLoc { token: Token::LiteralInt(120, LiteralIntType::U8), line: 6, col: 37, filename },
                TokenLoc { token: Token::LiteralInt(123456, LiteralIntType::Unknown), line: 7, col: 1, filename },
                TokenLoc { token: Token::LiteralInt(0xcafe, LiteralIntType::Unknown), line: 7, col: 8, filename },
                TokenLoc { token: Token::LiteralInt(0b101010, LiteralIntType::Unknown), line: 7, col: 15, filename },
                TokenLoc { token: Token::LiteralInt(0xbaba, LiteralIntType::U16), line: 7, col: 24, filename },
                TokenLoc { token: Token::LiteralInt(54, LiteralIntType::I8), line: 7, col: 34, filename },
                TokenLoc { token: Token::LiteralInt(0, LiteralIntType::U8), line: 7, col: 39, filename },
                TokenLoc { token: Token::LiteralInt(0xffe, LiteralIntType::I16), line: 7, col: 43, filename },
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
    fn bad_asm() {
        let filename = "test.li";
        assert_eq!(
            lex(" `ld blah  ", filename),
            Err(CompileError {
                loc: TokenLoc { token: Token::Unknown, line: 1, col: 2, filename },
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
                loc: TokenLoc { token: Token::Unknown, line: 1, col: 1, filename },
                msg: "Invalid integer suffix: 'u42'".to_string()
            })
        );
    }
}
