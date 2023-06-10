use crate::{bytecode::{ByteCode, Instruction}, CompilerResult};

use self::stack::{Stack, Value};

mod stack;

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================

macro_rules! int {
    ($e:expr) => { Value::Int($e) };
}

macro_rules! float {
    ($e:expr) => { Value::Float($e) };
}

macro_rules! bool {
    ($e:expr) => { Value::Bool($e) };
}

//======================================================================================
//          STRUCTURES
//======================================================================================

pub struct TrinityVM;

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl TrinityVM {
    pub fn execute(code: ByteCode, trace: bool, output: &mut impl std::io::Write) -> CompilerResult<()> {
        let ByteCode { code, constants_int, constants_float } = code;
        let mut pc = 0;
        let mut stack = Stack::new();

        if trace {
            println!("CONSTANTS INT:\n{}\n\nCONSTANTS FLOAT:\n{}\n", constants_int, constants_float);
        }

        loop {
            if trace {
                println!("Stack: {}\nNext: {}", stack, code[pc]);
            }

            match code[pc] {
                Instruction::LoadConstantInt { index } => {
                    stack.push(int!(constants_int[index as usize]))?;
                },

                Instruction::LoadConstantZeroInt => {
                    stack.push(int!(0))?;
                },

                Instruction::LoadConstantFloat { index } => {
                    stack.push(float!(constants_float[index as usize]))?;
                },

                Instruction::LoadConstantZeroFloat => {
                    stack.push(float!(0.0))?;
                },

                Instruction::LoadConstantBool(value) => {
                    stack.push(bool!(value))?;
                },

                Instruction::AddInt => {
                    let r = stack.pop().as_int();
                    let l = stack.pop().as_int();
                    stack.push(int!(l + r))?;
                },

                Instruction::AddFloat => {
                    let r = stack.pop().as_float();
                    let l = stack.pop().as_float();
                    stack.push(float!(l + r))?;
                },

                Instruction::SubInt => {
                    let r = stack.pop().as_int();
                    let l = stack.pop().as_int();
                    stack.push(int!(l - r))?;
                },

                Instruction::SubFloat => {
                    let r = stack.pop().as_float();
                    let l = stack.pop().as_float();
                    stack.push(float!(l - r))?;
                },

                Instruction::MulInt => {
                    let r = stack.pop().as_int();
                    let l = stack.pop().as_int();
                    stack.push(int!(l * r))?;
                },

                Instruction::MulFloat => {
                    let r = stack.pop().as_float();
                    let l = stack.pop().as_float();
                    stack.push(float!(l * r))?;
                },

                Instruction::DivInt => {
                    let r = stack.pop().as_int();
                    if r == 0 {
                        return Err("Division by zero".to_string());
                    }

                    let l = stack.pop().as_int();
                    stack.push(int!(l / r))?;
                },

                Instruction::DivFloat => {
                    let r = stack.pop().as_float();
                    let l = stack.pop().as_float();
                    stack.push(float!(l / r))?;
                },

                Instruction::ModInt => {
                    let r = stack.pop().as_int();
                    let l = stack.pop().as_int();
                    stack.push(int!(l % r))?;
                },

                Instruction::IsZeroInt => {
                    let value = stack.pop().as_int();
                    stack.push(bool!(value == 0))?;
                },

                Instruction::IsZeroFloat => {
                    let value = stack.pop().as_float();
                    stack.push(bool!(value == 0.0))?;
                },

                Instruction::IsPositiveInt => {
                    let value = stack.pop().as_int();
                    stack.push(bool!(value > 0))?;
                },

                Instruction::IsPositiveFloat => {
                    let value = stack.pop().as_float();
                    stack.push(bool!(value > 0.0))?;
                },

                Instruction::IsPositiveOrZeroInt => {
                    let value = stack.pop().as_int();
                    stack.push(bool!(value >= 0))?;
                },

                Instruction::IsPositiveOrZeroFloat => {
                    let value = stack.pop().as_float();
                    stack.push(bool!(value >= 0.0))?;
                },

                Instruction::IsEqualBool => {
                    let r = stack.pop().as_bool();
                    let l = stack.pop().as_bool();
                    stack.push(bool!(l == r))?;
                },

                Instruction::NotBool => {
                    let value = stack.pop().as_bool();
                    stack.push(bool!(!value))?;
                },

                Instruction::AndBool => {
                    let r = stack.pop().as_bool();
                    let l = stack.pop().as_bool();
                    stack.push(bool!(l && r))?;
                },
                
                Instruction::OrBool => {
                    let r = stack.pop().as_bool();
                    let l = stack.pop().as_bool();
                    stack.push(bool!(l || r))?;
                },

                Instruction::PrintInt => {
                    if trace {
                        print!(">>> ");
                    }
                    writeln!(output, "{}", stack.pop().as_int()).expect("internal write error");
                },

                Instruction::PrintFloat => {
                    if trace {
                        print!(">>> ");
                    }
                    writeln!(output, "{}", stack.pop().as_float()).expect("internal write error");
                },

                Instruction::PrintBool => {
                    if trace {
                        print!(">>> ");
                    }
                    writeln!(output, "{}", stack.pop().as_bool()).expect("internal write error");
                },

                Instruction::Pop => {
                    stack.pop();
                },

                Instruction::End => {
                    if stack.len() != 0 {
                        panic!("Program exiting, but stack not empty");
                    }
                    break;
                }
            }

            if trace {
                println!();
            }

            pc += 1;
        }

        Ok(())
    }
}