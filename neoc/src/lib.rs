use lalrpop_util::{lexer::Token, ParseError};

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod ast;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Calculator6Error {
    InputTooBig,
    OddNumber,
}

// Ok type is linkable byte code
pub fn compile<'input>(source: &'input str) -> Result<Box<ast::Expr>, ParseError<usize, Token<'input>, &'input str>> {
    let mut errors = Vec::new();
    parser::ProgramParser::new().parse(&mut errors, source)
}
