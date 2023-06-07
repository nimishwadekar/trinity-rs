use crate::{CompilerResult, err};
use super::ast::{ParseTree, Stmt, StmtType, Expr, ExprType, DataType};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================

macro_rules! is_type_eq {
    ($a:expr, $b:expr) => {
        $a.dtype() == $b.dtype()
    };
}

macro_rules! is_int {
    ($e:expr) => {
        $e.dtype() == DataType::Int
    };
}

macro_rules! is_float {
    ($e:expr) => {
        $e.dtype() == DataType::Float
    };
}

//======================================================================================
//          STRUCTURES
//======================================================================================

pub struct TypeChecker;

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl TypeChecker {
    pub fn parse(tree: &mut ParseTree)-> CompilerResult<()> {
        let parser = Self;
        for stmt in tree.stmts() {
            parser.parse_stmt(stmt)?;
        }
        Ok(())
    }

    fn parse_stmt(&self, stmt: &Stmt) -> CompilerResult<()> {
        match stmt.stmt() {
            StmtType::Expr(expr) => self.parse_expr(expr)?,
            StmtType::Print(expr) => {
                self.parse_expr(expr)?;
                if !is_int!(expr) && !is_float!(expr) {
                    return err!("Invalid operand type for `print`", expr.lexeme());
                }
            },
        };
        Ok(())
    }

    fn parse_expr(&self, expr: &Expr) -> CompilerResult<()> {
        match expr.expr() {
            ExprType::IntegerLiteral(..) => expr.set_dtype(DataType::Int),
            ExprType::FloatLiteral(..) => expr.set_dtype(DataType::Float),
            
            ExprType::Add { l, r } => {
                self.parse_expr(l)?;
                self.parse_expr(r)?;
                
                let dtype = if is_int!(l) && is_int!(r) || is_float!(l) && is_float!(r) {
                    l.dtype()
                } else {
                    return err!(format!("Invalid operands of types `{}` and `{}` for `+`", l.dtype(), r.dtype()), r.lexeme());
                };

                expr.set_dtype(dtype);
            },

            ExprType::Positive(operand) => {
                self.parse_expr(operand)?;
                
                let dtype = if is_int!(operand) || is_float!(operand) {
                    operand.dtype()
                } else {
                    return err!(format!("Invalid operand of type {} for `+`", operand.dtype()), operand.lexeme());
                };

                expr.set_dtype(dtype);
            }
        };

        Ok(())
    }
}