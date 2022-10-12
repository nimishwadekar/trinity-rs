use crate::{
    ast::Ast,
    code::ByteCode,
    error::CompilationError,
};
use lalrpop_util::{
    ErrorRecovery,
    lexer::Token,
};

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! debug {
    ($x:expr) => { dbg!($x) }
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! debug {
    ($x:expr) => { std::convert::identity($x) }
}

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

    ByteCode::generate_code(ast)
}
