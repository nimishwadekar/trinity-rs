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

pub fn compile<'input>(source: &'input str) -> Result<LinkableByteCode, CompilationError> {
    
    // Source to AST

    let mut errors: Vec<ErrorRecovery<usize, Token, &str>> = Vec::new();
    let ast: Ast = match parser::ProgramParser::new().parse(&mut errors, source) {
        Ok(ast) => ast,
        Err(e) => {
            use lalrpop_util::ParseError::*;
            return Err(match e {
                InvalidToken { location: _ } => CompilationError::InvalidToken,
                e => panic!("Some error that should not have come here: {:?}", e),
            });
        },
    };

    debug!(&ast);
    debug!(&errors);

    if errors.len() > 0 {
        // Parse errors and return error information.
        return Err(CompilationError::UnrecognizedToken); // TEMP return.
    }

    // AST to Byte code

    debug!(ByteCode::generate_code(ast))
}
