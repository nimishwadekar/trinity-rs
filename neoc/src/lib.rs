use crate::{ast::Ast, code::ByteCode};
use lalrpop_util::{
    ErrorRecovery,
    lexer::Token,
};
use neo_util::{LinkableByteCode, debug};

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod ast;
mod code;
mod error;

pub use error::CompilationError;

pub fn compile<'input>(source: &'input str) -> Result<LinkableByteCode, Vec<CompilationError>> {
    
    // Source to AST

    let mut errors: Vec<ErrorRecovery<usize, Token, CompilationError>> = Vec::new();
    let ast: Ast = match parser::ProgramParser::new().parse(&mut errors, source) {
        Ok(ast) => ast,
        Err(e) => {
            let mut errors = error::parse_errors(errors, source);
            errors.push(CompilationError::from(e, source));
            return Err(errors);
        },
    };

    if errors.len() > 0 {
        // Parse errors and return error information.
        return Err(error::parse_errors(errors, source));
    }

    debug!(&ast);

    // AST to Byte code

    match ByteCode::generate_code(ast) {
        Err(e) => Err(vec![e]),
        Ok(code) => Ok(debug!(code)),
    }
}
