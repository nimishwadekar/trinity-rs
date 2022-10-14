use crate::{
    ast::{
        Ast,
        Stmt,
        Expr, 
        BinaryOpType,
    },
    error::CompilationError::{
        self,
        *,
    },
};
use neo_util::{Instruction::*, LinkableByteCode};

//=========================================
// TYPES
//=========================================

pub struct ByteCode(LinkableByteCode);

//=========================================
// IMPLEMENTATIONS
//=========================================

#[allow(non_snake_case)]
impl ByteCode {
    /// Traverses the AST to generate byte code.
    pub fn generate_code(ast: Ast) -> Result<LinkableByteCode, CompilationError> {
        let mut byte_code = Self (LinkableByteCode {
            code: Vec::new(),
            constants: Vec::new(),
        });

        for stmt in ast.unwrap() {
            byte_code.generate_stmt_tree_code(&stmt)?;
        }

        Ok(byte_code.0)
    }

    //=========================================
    // STMT
    //=========================================

    fn generate_stmt_tree_code(&mut self, stmt: &Box<Stmt>) -> Result<(), CompilationError> {
        use Stmt::*;
        match stmt.as_ref() {
            Expr(expr) => {
                self.generate_expr_tree_code(expr)?;
                self.0.code.push(Pop);
                Ok(())
            },
            Nop => Ok(()),
        }
    }

    //=========================================
    // EXPR
    //=========================================

    fn generate_expr_tree_code(&mut self, expr: &Box<Expr>) -> Result<(), CompilationError> {
        use Expr::*;
        match expr.as_ref() {
            BinaryOp(op, l, r) => {
                self.generate_expr_tree_code(l)?;
                self.generate_expr_tree_code(r)?;
                self.generate_expr_BinaryOp(op)
            },
            Int32(i) => self.generate_expr_Int32(i),
            Error => panic!("FATAL: Should never have reached code generation phase."),
        }
    }
    
    fn generate_expr_BinaryOp(&mut self, op: &BinaryOpType) -> Result<(), CompilationError> {
        self.0.code.push(match op {
            BinaryOpType::Add => Add,
            BinaryOpType::Sub => Sub,
            BinaryOpType::Mul => Mul,
            BinaryOpType::Div => Div,
        });
        Ok(())
    }

    fn generate_expr_Int32(&mut self, i: &i32) -> Result<(), CompilationError> {
        let index = self.0.constants.len() + 1;
        if index > u8::MAX as usize {
            return Err(TooManyConstants);
        }

        self.0.constants.push(*i);
        self.0.code.push(Load{index: index as u8});
        Ok(())
    }
}