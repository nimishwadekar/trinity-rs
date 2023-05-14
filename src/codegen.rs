use crate::{
    parser::stage1::{
        Stage1Tree,
        Stmt, Expr,
    },
    bytecode::{ByteCode, Instruction},
};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================

pub struct CodeGenerator {
    code: ByteCode,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl CodeGenerator {
    pub fn generate(parsed_program: Stage1Tree) -> Result<ByteCode, String> {
        let mut codegen = CodeGenerator { code: ByteCode::new() };
        codegen.generate_program(parsed_program)?;
        Ok(codegen.code)
    }

    fn generate_program(&mut self, parsed_program: Stage1Tree) -> Result<(), String> {
        for stmt in parsed_program.stmts() {
            self.generate_stmt(stmt)?;
        }
        self.code.write_instruction(Instruction::End);
        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expr(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(Instruction::Pop);
            },
            Stmt::Print(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(Instruction::Print);
            }
        };
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::IntegerLiteral(value) => {
                let index = self.code.add_constant(*value)?;
                self.code.write_instruction(Instruction::LoadConstant { index });
            },

            Expr::Positive(expr) => {
                self.generate_expr(expr)?;
            },

            Expr::Add { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(Instruction::Add);
            }
        };
        Ok(())
    }
}