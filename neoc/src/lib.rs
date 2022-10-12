use lalrpop_util::{lexer::Token, ErrorRecovery};
use code::{ByteCodeGenerator, ByteCode};
use error::CompilationError;

use crate::{
    ast::{Ast, Expr},
};

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod ast;
mod code;
mod error;

pub fn compile<'input>(source: &'input str) -> Result<ByteCode, CompilationError> {
    
    // Source to AST

    let mut errors: Vec<ErrorRecovery<usize, Token, &str>> = Vec::new();
    let ast: Ast = match parser::ProgramParser::new().parse(&mut errors, source) {
        Ok(ast) => ast,
        Err(e) => {
            use lalrpop_util::ParseError::*;
            return Err(match e {
                InvalidToken { location } => CompilationError::InvalidToken,
                e => panic!("Some error that should not have come here: {:?}", e),
            });
        },
    };

    ast.pretty_print_stdout();
    dbg!(&errors);

    if errors.len() > 0 {
        // Parse errors and return error information.
        return Err(CompilationError::UnrecognizedToken); // TEMP empty return.
    }

    // AST to Byte code

    ByteCodeGenerator::generate_code(ast)
}
