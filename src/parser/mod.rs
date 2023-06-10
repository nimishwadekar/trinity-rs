use crate::{lexer::TokenStream, CompilerResult};

/// The AST definition.
mod ast;

/// Constructs the AST.
mod constructor;

/// The Symbol Table definition.
mod table;

/// Type-checks the AST.
mod type_checker;

pub use ast::{Expr, ExprType, Stmt, StmtType, ParseTree, DataType};
pub use table::SymbolTable;

#[macro_export]
macro_rules! err {
    ($msg:expr, $lexeme:expr) => {
        Err(format!("<{}> `{}`: {}", $lexeme.location(), $lexeme, $msg))
    };
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
    symbols: SymbolTable,
}

impl<'a> Parser<'a> {
    // Parsing stages:
    // 1. Construst syntax tree.
    // 2. Resolve identifiers.
    // 3. Perform type checking.
    pub fn parse(tokens: TokenStream<'a>)-> CompilerResult<(ParseTree, SymbolTable)> {
        let mut parser = Self::new(tokens);
        let mut tree = parser.construct()?;
        parser.type_check(&mut tree)?;
        parser.symbols.lock();
        println!("{}", parser.symbols);
        Ok((tree, parser.symbols))
    }

    fn new(tokens: TokenStream<'a>) -> Self {
        Self {
            tokens,
            symbols: SymbolTable::new(),
        }
    }
}