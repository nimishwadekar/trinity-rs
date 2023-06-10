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
    pub fn type_check(&mut self, tree: &mut ParseTree)-> CompilerResult<()> {
        for stmt in tree.stmts() {
            self.typecheck_stmt(stmt)?;
        }
        Ok(())
    }

    fn typecheck_stmt(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        match stmt.stmt() {
            StmtType::Expr(expr) => self.typecheck_expr(expr)?,

            StmtType::Let { identifier, dtype, initialiser } => {
                self.typecheck_expr(initialiser)?;
                if initialiser.dtype() != *dtype {
                    return err!("`let` initialiser type mismatch", identifier);
                }
                self.symbols.insert(identifier.clone(), *dtype)?;
            },

            StmtType::Print(expr) => {
                self.typecheck_expr(expr)?;
                if !expr.dtype().is_primitive() {
                    return err!("Invalid operand type for `print`", expr.lexeme());
                }
            },
        };
        Ok(())
    }

    fn typecheck_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr.expr() {
            ExprType::IntegerLiteral(..) => expr.set_dtype(DataType::Int),
            ExprType::FloatLiteral(..) => expr.set_dtype(DataType::Float),
            ExprType::BoolLiteral(..) => expr.set_dtype(DataType::Bool),
            
            ExprType::Identifier => {
                match self.symbols.get(expr.lexeme()) {
                    Some(entry) => expr.set_dtype(entry.dtype),
                    None => return err!("Undefined identifier", expr.lexeme()),
                }
            }

            ExprType::Block(stmts, ending_expr) => {
                self.symbols.open_scope();
                for stmt in stmts {
                    self.typecheck_stmt(stmt)?;
                }
                match ending_expr {
                    Some(ending_expr) => {
                        self.typecheck_expr(ending_expr)?;
                        expr.set_dtype(ending_expr.dtype())
                    },
                    None => expr.set_dtype(DataType::Unit),
                }
                self.symbols.close_scope();
            }
            
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
                self.typecheck_expr(l)?;
                self.typecheck_expr(r)?;
                
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
                self.typecheck_expr(operand)?;
                
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