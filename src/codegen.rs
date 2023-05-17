use crate::{
    parser::{ParseTree, Expr, ExprType, Stmt, StmtType},
    bytecode::{ByteCode, Instruction},
    CompilerResult,
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
    pub fn generate(parsed_program: ParseTree) -> CompilerResult<ByteCode> {
        let mut codegen = CodeGenerator { code: ByteCode::new() };
        codegen.generate_program(parsed_program)?;
        Ok(codegen.code)
    }

    fn generate_program(&mut self, parsed_program: ParseTree) -> CompilerResult<()> {
        for stmt in parsed_program.stmts() {
            self.generate_stmt(stmt)?;
        }
        self.code.write_instruction(Instruction::End);
        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        match stmt.stmt() {
            StmtType::Expr(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(Instruction::Pop);
            },
            StmtType::Print(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(Instruction::Print);
            }
        };
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr.expr() {
            ExprType::IntegerLiteral(value) => {
                let index = self.code.add_constant(value.as_str().parse::<i32>().unwrap())?;
                self.code.write_instruction(Instruction::LoadConstant { index });
            },

            ExprType::Positive(expr) => {
                self.generate_expr(expr)?;
            },

            ExprType::Add { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(Instruction::Add);
            }
        };
        Ok(())
    }
}