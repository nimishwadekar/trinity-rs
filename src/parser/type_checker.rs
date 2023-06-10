use crate::{CompilerResult, err};
use super::ast::{ParseTree, Stmt, StmtType, Expr, ExprType, DataType};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



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
                if !expr.dtype().is_primitive() {
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
            ExprType::BoolLiteral(..) => expr.set_dtype(DataType::Bool),
            
            ExprType::Add { l, r }
            | ExprType::Subtract { l, r }
            | ExprType::Multiply { l, r }
            | ExprType::Divide { l, r }
            | ExprType::Remainder { l, r }
            
            | ExprType::Lesser { l, r }
            | ExprType::LesserEqual { l, r }
            | ExprType::Greater { l, r }
            | ExprType::GreaterEqual { l, r }
            | ExprType::Equal { l, r }
            | ExprType::NotEqual { l, r }
            
            | ExprType::And { l, r }
            | ExprType::Or { l, r } => {
                self.parse_expr(l)?;
                self.parse_expr(r)?;
                
                let dtype = if match expr.expr() {
                    ExprType::Add {..}
                    | ExprType::Subtract {..}
                    | ExprType::Multiply {..}
                    | ExprType::Divide {..}
                    | ExprType::Lesser {..}
                    | ExprType::LesserEqual {..}
                    | ExprType::Greater {..}
                    | ExprType::GreaterEqual {..} => l.dtype().is_numeric_primitive(),
                    
                    ExprType::Equal {..}
                    | ExprType::NotEqual {..} => l.dtype().is_primitive(),

                    ExprType::Remainder {..} => l.dtype().is_int(),
                    
                    ExprType::And {..}
                    | ExprType::Or {..} => l.dtype().is_bool(),

                    _ => unreachable!(),
                } && l.dtype() == r.dtype() {
                    match expr.expr() {
                        ExprType::Add {..}
                        | ExprType::Subtract {..}
                        | ExprType::Multiply {..}
                        | ExprType::Divide {..}
                        | ExprType::Remainder {..} => l.dtype(),
                        
                        ExprType::Lesser {..}
                        | ExprType::LesserEqual {..}
                        | ExprType::Greater {..}
                        | ExprType::GreaterEqual {..}
                        | ExprType::Equal {..}
                        | ExprType::NotEqual {..}
                        | ExprType::And {..}
                        | ExprType::Or {..} => DataType::Bool,
    
                        _ => unreachable!(),
                    }
                } 
                else {
                    return err!(format!("Invalid operands of types `{}` and `{}`", l.dtype(), r.dtype()), r.lexeme());
                };

                expr.set_dtype(dtype);
            },

            ExprType::Positive(operand)
            | ExprType::Negative(operand)
            | ExprType::Not(operand) => {
                self.parse_expr(operand)?;
                
                let dtype = if match expr.expr() {
                    ExprType::Positive(..)
                    | ExprType::Negative(..) => operand.dtype().is_numeric_primitive(),
                    ExprType::Not(..) => operand.dtype().is_bool(),
                    _ => unreachable!(),
                } {
                    operand.dtype()
                }
                else {
                    return err!(format!("Invalid operand of type `{}`", operand.dtype()), operand.lexeme());
                };

                expr.set_dtype(dtype);
            }
        };

        Ok(())
    }
}