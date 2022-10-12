use crate::{
    ast::{
        Ast,
        Expr,
    },
    error::CompilationError,
};

//=========================================
// TYPES
//=========================================

#[derive(Debug)]
pub enum Instruction {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct ByteCode {
    // Structure to return after generation.
}

pub struct ByteCodeGenerator {
    code: Vec<Instruction>,
    ast: Ast,
}


//=========================================
// IMPLEMENTATIONS
//=========================================

impl ByteCodeGenerator {
    /// Traverses the AST to generate byte code.
    pub fn generate_code(ast: Ast) -> Result<ByteCode, CompilationError> {
        let mut generator = Self {
            code: Vec::new(),
            ast,
        };

        Ok(ByteCode {})
    }
}