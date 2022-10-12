use core::fmt::Debug;

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
use Instruction::*;

//=========================================
// TYPES
//=========================================

pub enum Instruction {
    // Operations
    Add,
    Sub,
    Mul,
    Div,

    // Stack Manipulation
    /// Loads the constant at `index` in the constant pool
    Load{index: u8},
}

pub struct ByteCode {
    code: Vec<Instruction>,
    constants: Vec<i32>,
}


//=========================================
// IMPLEMENTATIONS
//=========================================

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Mul => write!(f, "mul"),
            Div => write!(f, "div"),

            Load { index } => write!(f, "load {:?}", index),
        }
    }
}

impl Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "ByteCode {{\nCode:")?;
        for (i, instruction) in self.code.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", instruction)?;
        }
        writeln!(f, "\nConstants:")?;
        for (i, &constant) in self.constants.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", constant)?;
        }
        Ok(())
    }
}

impl ByteCode {
    /// Traverses the AST to generate byte code.
    pub fn generate_code(ast: Ast) -> Result<ByteCode, CompilationError> {
        let mut byte_code = Self {
            code: Vec::new(),
            constants: Vec::new(),
        };

        for stmt in ast.unwrap() {
            byte_code.generate_stmt_tree_code(&stmt)?;
        }

        Ok(byte_code)
    }

    //=========================================
    // STMT
    //=========================================

    fn generate_stmt_tree_code(&mut self, stmt: &Box<Stmt>) -> Result<(), CompilationError> {
        use Stmt::*;
        match stmt.as_ref() {
            Expr(expr) => self.generate_expr_tree_code(expr),
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
        self.code.push(match op {
            BinaryOpType::Add => Add,
            BinaryOpType::Sub => Sub,
            BinaryOpType::Mul => Mul,
            BinaryOpType::Div => Div,
        });
        Ok(())
    }

    fn generate_expr_Int32(&mut self, i: &i32) -> Result<(), CompilationError> {
        let index = self.constants.len() + 1;
        if index > u8::MAX as usize {
            return Err(TooManyConstants);
        }

        self.constants.push(*i);
        self.code.push(Load{index: index as u8});
        Ok(())
    }
}