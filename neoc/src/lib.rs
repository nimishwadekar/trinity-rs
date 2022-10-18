use crate::{parsetree::ParseTree, ast::Ast, code::ByteCodeGenerator};
use lalrpop_util::{
    ErrorRecovery,
    lexer::Token,
};
use neo_util::LinkableByteCode;

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod parsetree;
mod ast;
mod types;
mod code;
mod error;

pub use error::NeoCError;

pub fn compile<'input>(source: &'input str) -> Result<LinkableByteCode, Vec<NeoCError>> {
    
    // Source to Parse Tree

    let mut errors: Vec<ErrorRecovery<usize, Token, error::ParseError>> = Vec::new();
    let parse_tree: ParseTree = match parser::ProgramParser::new().parse(&mut errors, source) {
        Ok(parse_tree) => parse_tree,
        Err(e) => {
            let mut errors = error::parse_errors(errors, source);
            errors.push(NeoCError::Parse(error::ParseError::from(e, source)));
            return Err(errors);
        },
    };

    if errors.len() > 0 {
        // Parse errors and return error information.
        return Err(error::parse_errors(errors, source));
    }

    eprintln!("{:?}", &parse_tree);
    eprintln!("-------------------------------------------------------------");

    // Parse Tree to AST

    let ast = match Ast::from_parse_tree(parse_tree) {
        Ok(ast) => ast,
        Err(e) => {
            return Err(vec![NeoCError::Compile(e)]);
        }
    };

    eprintln!("{:?}", &ast);
    eprintln!("-------------------------------------------------------------");

    // AST to Byte code

    match ByteCodeGenerator::generate_code(ast) {
        Err(e) => Err(vec![NeoCError::Compile(e)]),
        Ok(code) => Ok(dbg!(code)),
    }
}
