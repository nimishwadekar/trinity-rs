use crate::{
    parser::{
        ParsedProgram,
        Stmt, Expr, ExprOperator,
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
    pub fn generate(parsed_program: ParsedProgram) -> Result<ByteCode, String> {
        let mut codegen = CodeGenerator { code: ByteCode::new() };
        codegen.generate_program(parsed_program)?;
        Ok(codegen.code)
    }

    fn generate_program(&mut self, parsed_program: ParsedProgram) -> Result<(), String> {
        for stmt in parsed_program.stmts() {
            self.generate_stmt(stmt)?;
        }
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
            // Modify tree so that this is handled before this.
            Expr::Operation { operator, operands } => {
                match operator {
                    ExprOperator::Add => {
                        self.generate_expr(&operands[0])?;
                        self.generate_expr(&operands[1])?;
                        self.code.write_instruction(Instruction::Add);
                    }
                    o => todo!("{:?}", o),
                }
            },
        };
        Ok(())
    }
}