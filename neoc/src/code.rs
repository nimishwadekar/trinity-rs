use crate::{
    ast::{
        Ast,
        Stmt,
        TypedExpr,
        Expr,
        Literal,
        ArithmeticBinOpType,
    },
    error::CompileError,
    types::DataType,
};
use neo_util::{Instruction, LinkableByteCode};

//=========================================
// TYPES
//=========================================

pub struct ByteCodeGenerator{
    byte_code: LinkableByteCode,
    //dtype_stack: Vec<DataType>,
}

//=========================================
// IMPLEMENTATIONS
//=========================================

#[allow(non_snake_case)]
impl ByteCodeGenerator {
    /// Traverses the AST to generate byte code.
    pub fn generate_code(ast: Ast) -> Result<LinkableByteCode, CompileError> {
        let mut byte_code = Self::new();

        for stmt in ast.unwrap() {
            byte_code.generate_stmt_tree_code(&stmt)?;
        }

        Ok(byte_code.byte_code)
    }

    //=========================================
    // STMT
    //=========================================

    fn generate_stmt_tree_code(&mut self, stmt: &Box<Stmt>) -> Result<(), CompileError> {
        Ok(match stmt.as_ref() {
            Stmt::Expr(expr) => {
                self.generate_expr_tree_code(expr)?;
                self.push_code(Instruction::Pop);
            },

            Stmt::Print(expr) => {
                self.generate_expr_tree_code(expr)?;
                self.push_code(match expr.dtype {
                    DataType::Int => Instruction::iPrint,
                    DataType::Bool => Instruction::bPrint,
                    DataType::Float => Instruction::fPrint,
                    DataType::Nil => Instruction::nPrint,
                });
            },

            Stmt::Nop => (),
        })
    }

    //=========================================
    // EXPR
    //=========================================

    fn generate_expr_tree_code(&mut self, expr: &Box<TypedExpr>) -> Result<(), CompileError> {
        let TypedExpr { dtype, expr } = expr.as_ref();
        match expr {
            Expr::ArithmeticBinOp(op, l, r) => {
                self.generate_expr_tree_code(l)?;
                self.generate_expr_tree_code(r)?;
                self.generate_expr_ArithmeticBinOp(op, dtype)
            },

            Expr::Literal(val) => self.generate_expr_Literal(val),
        }
    }
    
    fn generate_expr_ArithmeticBinOp(&mut self, op: &ArithmeticBinOpType, dtype: &DataType) -> Result<(), CompileError> {
        self.push_code(match dtype {
            DataType::Int => match op {
                ArithmeticBinOpType::Add => Instruction::iAdd,
                ArithmeticBinOpType::Sub => Instruction::iSub,
                ArithmeticBinOpType::Mul => Instruction::iMul,
                ArithmeticBinOpType::Div => Instruction::iDiv,
            },

            DataType::Float => match op {
                ArithmeticBinOpType::Add => Instruction::fAdd,
                ArithmeticBinOpType::Sub => Instruction::fSub,
                ArithmeticBinOpType::Mul => Instruction::fMul,
                ArithmeticBinOpType::Div => Instruction::fDiv,
            },

            t => unreachable!("DataType {} should not have reached code generation for ArithmeticBinOp.", t.to_string()),
        });
        Ok(())
    }

    fn generate_expr_Literal(&mut self, val: &Literal) -> Result<(), CompileError> {
        const I_CONSTS: [Instruction; 2] = [
            Instruction::iConst_0,
            Instruction::iConst_1,
        ];

        match val {
            Literal::Int(val) => {
                if *val >= 0 && (*val as usize) < I_CONSTS.len() {
                    self.push_code(I_CONSTS[*val as usize]);
                    return Ok(())
                }

                let data = unsafe { std::mem::transmute::<i64, u64>(*val) };
                let offset = self.push_constant(data);
                if offset > u8::MAX as usize { return Err(CompileError::TooManyConstants) }
                self.push_code(Instruction::Const { offset: offset as u8 });
            },

            Literal::Bool(val) => {
                self.push_code(match val {
                    true => Instruction::iConst_1,
                    false => Instruction::iConst_0,
                });
            },

            Literal::Float(val) => {
                let data = unsafe { std::mem::transmute::<f64, u64>(*val) };
                let offset = self.push_constant(data);
                if offset > u8::MAX as usize { return Err(CompileError::TooManyConstants) }
                self.push_code(Instruction::Const { offset: offset as u8 });
            },

            Literal::Nil => self.push_code(Instruction::iConst_0),
        }
        Ok(())
    }
}

// For trivial wrappers over `LinkableByteCode`.
impl ByteCodeGenerator {
    fn new() -> Self {
        Self {
            byte_code: LinkableByteCode::new(),
        }
    }

    fn push_code(&mut self, instr: Instruction) { self.byte_code.push_code(instr) }

    /// Returns the offset the data is at.
    fn push_constant(&mut self, data: u64) -> usize { self.byte_code.push_constant(data) }
}