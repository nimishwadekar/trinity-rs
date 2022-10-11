use lalrpop_util::{lexer::Token, ErrorRecovery};
use types::Expr;

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod types;

#[derive(Debug)]
pub struct ParseResult<'input> {
    pub ast: Box<Expr>,
    pub errors: Option<Vec<ErrorRecovery<usize, Token<'input>, &'input str>>>,
}

impl<'input> ParseResult<'input> {
    pub fn new(ast: Box<Expr>, errors: Option<Vec<ErrorRecovery<usize, Token<'input>, &'input str>>>) -> Self {
        Self { ast, errors }
    }
}

pub fn compile<'input>(source: &'input str) -> ParseResult {
    let mut errors = Vec::new();
    let ast = parser::ProgramParser::new().parse(&mut errors, source)
        .expect("FATAL: `parse()` should absolutely have not returned an `Err` variant");
    if errors.len() > 0 {
        ParseResult::new(ast, Some(errors))
    }
    else {
        ParseResult::new(ast, None)
    }
}
