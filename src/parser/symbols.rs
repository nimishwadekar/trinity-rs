use crate::{CompilerResult, err};
use super::{ast::{ParseTree, Stmt, StmtType, Expr, ExprType, DataType}, Parser};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================



//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> Parser<'a> {
    pub fn resolve_symbols(&mut self, tree: &mut ParseTree)-> CompilerResult<()> {
        for stmt in tree.stmts() {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        match stmt.stmt() {
            StmtType::Let { identifier, dtype, initialiser } => {
                /* self.resolve_expr(initialiser)?;
                self.symbols.insert(identifier.clone(), *dtype); */
            },

            _ => (),
        };
        Ok(())
    }

    fn resolve_expr(&self, expr: &Expr) -> CompilerResult<()> {
        match expr.expr() {
            ExprType::Identifier => {
                /* if let None = self.symbols.get(expr.lexeme()) {
                    return err!("Undefined identifier", expr.lexeme());
                } */
            },

            _ => (),
        }
        Ok(())
    }
}