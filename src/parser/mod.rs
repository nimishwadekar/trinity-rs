use crate::{lexer::TokenStream, CompilerResult};

use self::{
    constructor::Constructor,
    type_checker::TypeChecker,
};

/// The AST definition.
mod ast;

/// Constructs the AST.
mod constructor;

/// Type-checks the AST.
mod type_checker;

pub use ast::{Expr, ExprType, Stmt, StmtType, ParseTree, DataType};

#[macro_export]
macro_rules! err {
    ($msg:expr, $lexeme:expr) => {
        Err(format!("<{}> `{}`: {}", $lexeme.location(), $lexeme, $msg))
    };
}

pub struct Parser;

impl Parser {
    // Parsing stages:
    // 1. Construst syntax tree.
    // 2. Perform type checking.
    pub fn parse<'a>(tokens: TokenStream<'a>)-> CompilerResult<ParseTree> {
        let mut tree = Constructor::parse(tokens)?;
        TypeChecker::parse(&mut tree)?;
        Ok(tree)
    }
}