use crate::{
    parser::{ParseTree, Expr, ExprType, Stmt, StmtType, DataType, SymbolTable},
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
    table: SymbolTable,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl CodeGenerator {
    pub fn generate(parsed_program: ParseTree, symbol_table: SymbolTable) -> CompilerResult<ByteCode> {
        let mut codegen = CodeGenerator {
            code: ByteCode::new(),
            table: symbol_table,
        };
        codegen.generate_program(parsed_program)?;
        codegen.code.max_identifiers_in_scope = codegen.table.max_identifiers_in_scope();
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

            StmtType::Let { identifier, initialiser, .. } => {
                self.generate_expr(initialiser)?;
                let entry = self.table.get(identifier).expect("Table does not have entry for identifier");
                let index = entry.index;
                self.code.write_instruction(Instruction::StoreVariable { index });
            },

            StmtType::Print(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::PrintInt,
                    DataType::Float => Instruction::PrintFloat,
                    DataType::Bool => Instruction::PrintBool,
                    t => unreachable!("Invalid print {t}"),
                });
            }
        };
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr.expr() {
            ExprType::IntegerLiteral(value) => {
                let instruction = match *value {
                    0 => Instruction::LoadConstantZeroInt,
                    _ => Instruction::LoadConstantInt { index: self.code.insert_constant_int(*value)? },
                };
                self.code.write_instruction(instruction);
            },

            ExprType::FloatLiteral(value) => {
                let instruction = if *value == 0.0 {
                    Instruction::LoadConstantZeroFloat
                } else {
                    Instruction::LoadConstantFloat { index: self.code.insert_constant_float(*value)? }
                };
                self.code.write_instruction(instruction);
            },

            ExprType::BoolLiteral(value) => self.code.write_instruction(Instruction::LoadConstantBool(*value)),

            ExprType::Identifier => {
                let entry = self.table.get(expr.lexeme()).unwrap();
                let index = entry.index;
                self.code.write_instruction(Instruction::LoadVariable { index });
            },

            ExprType::Block(stmts, expr) => {
                self.table.open_scope();
                for stmt in stmts {
                    self.generate_stmt(stmt)?;
                }
                match expr {
                    Some(expr) => self.generate_expr(expr)?,
                    None => self.code.write_instruction(Instruction::LoadUnit),
                }
                self.table.close_scope();
            }

            ExprType::Positive(expr) => {
                self.generate_expr(expr)?;
            },

            ExprType::Negative(expr) => {
                match expr.dtype() {
                    DataType::Int => {
                        self.code.write_instruction(Instruction::LoadConstantZeroInt);
                        self.generate_expr(expr)?;
                        self.code.write_instruction(Instruction::SubInt);
                    },
                    DataType::Float => {
                        self.code.write_instruction(Instruction::LoadConstantZeroFloat);
                        self.generate_expr(expr)?;
                        self.code.write_instruction(Instruction::SubFloat);
                    },
                    t => unreachable!("Invalid Negative {t}"),
                };
            },

            ExprType::Add { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::AddInt,
                    DataType::Float => Instruction::AddFloat,
                    t => unreachable!("Invalid Add {t}"),
                });
            },

            ExprType::Subtract { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::SubInt,
                    DataType::Float => Instruction::SubFloat,
                    t => unreachable!("Invalid Sub {t}"),
                });
            },

            ExprType::Multiply { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::MulInt,
                    DataType::Float => Instruction::MulFloat,
                    t => unreachable!("Invalid Mul {t}"),
                });
            },

            ExprType::Divide { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::DivInt,
                    DataType::Float => Instruction::DivFloat,
                    t => unreachable!("Invalid Div {t}"),
                });
            },

            ExprType::Remainder { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(match expr.dtype() {
                    DataType::Int => Instruction::ModInt,
                    t => unreachable!("Invalid Mod {t}"),
                });
            },

            ExprType::Lesser { l, r }
            | ExprType::LesserEqual { l, r }
            | ExprType::Greater { l, r }
            | ExprType::GreaterEqual { l, r }
            | ExprType::Equal { l, r }
            | ExprType::NotEqual { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                match l.dtype() {
                    DataType::Int => self.code.write_instruction(Instruction::SubInt),
                    DataType::Float => self.code.write_instruction(Instruction::SubFloat),
                    DataType::Bool => (),
                    t => unreachable!("Invalid Sub {t}"),
                };

                self.code.write_instruction(match expr.expr() {
                    ExprType::Lesser {..}
                    | ExprType::GreaterEqual {..} => match l.dtype() {
                        DataType::Int => Instruction::IsPositiveOrZeroInt,
                        DataType::Float => Instruction::IsPositiveOrZeroFloat,
                        t => unreachable!("Invalid IsPositiveOrZero {t}"),
                    },

                    ExprType::LesserEqual {..}
                    | ExprType::Greater {..} => match l.dtype() {
                        DataType::Int => Instruction::IsPositiveInt,
                        DataType::Float => Instruction::IsPositiveFloat,
                        t => unreachable!("Invalid IsPositive {t}"),
                    },

                    ExprType::Equal {..}
                    | ExprType::NotEqual {..} => match l.dtype() {
                        DataType::Int => Instruction::IsZeroInt,
                        DataType::Float => Instruction::IsZeroFloat,
                        DataType::Bool => Instruction::IsEqualBool,
                        t => unreachable!("Invalid IsZero {t}"),
                    },
                    _ => unreachable!(),
                });

                match expr.expr() {
                    ExprType::Lesser {..}
                    | ExprType::LesserEqual {..}
                    | ExprType::NotEqual {..} => self.code.write_instruction(Instruction::NotBool),
                    _ => (),
                };
            },

            ExprType::Not(expr) => {
                self.generate_expr(expr)?;
                self.code.write_instruction(Instruction::NotBool);
            },

            ExprType::And { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(Instruction::AndBool);
            },
            
            ExprType::Or { l, r } => {
                self.generate_expr(l)?;
                self.generate_expr(r)?;
                self.code.write_instruction(Instruction::OrBool);
            },
        };
        Ok(())
    }
}