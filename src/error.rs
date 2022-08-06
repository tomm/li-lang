use crate::lex::TokenLoc;

pub fn fatal_error(e: &CompileError) {
    eprintln!("{}:{}:{}: {}", e.loc.filename, e.loc.line, e.loc.col, e.msg);
}

#[derive(Debug,PartialEq,Eq)]
pub struct CompileError<'a> {
    pub loc: TokenLoc<'a>,
    pub msg: String
}
