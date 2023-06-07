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
    pub fn execute(code: ByteCode, trace: bool) -> CompilerResult<()> {
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

                Instruction::LoadConstantFloat { index } => {
                    stack.push(float!(constants_float[index as usize]))?;
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

                Instruction::PrintInt => {
                    if trace {
                        print!(">>> ");
                    }
                    println!("{}", stack.pop().as_int());
                },

                Instruction::PrintFloat => {
                    if trace {
                        print!(">>> ");
                    }
                    println!("{}", stack.pop().as_float());
                },

                Instruction::Pop => {
                    stack.pop();
                },

                Instruction::End => {
                    if stack.len() != 0 {
                        return Err("Program exiting, but stack not empty".to_string());
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