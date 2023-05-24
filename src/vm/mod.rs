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
        let ByteCode { code, constants } = code;
        let mut pc = 0;
        let mut stack = Stack::new();

        if trace {
            println!("CONSTANTS:\n{}\n", constants);
        }

        let constants = constants.to_vec();

        loop {
            if trace {
                println!("Stack: {}\nNext: {}", stack, code[pc]);
            }

            match code[pc] {
                Instruction::LoadConstant { index } => {
                    stack.push(int!(constants[index as usize]))?;
                },

                Instruction::Add => {
                    let r = stack.pop().as_int();
                    let l = stack.pop().as_int();
                    stack.push(int!(l + r))?;
                },

                Instruction::Print => {
                    if trace {
                        print!(">>> ");
                    }
                    println!("{}", stack.pop().as_int());
                },

                Instruction::Pop => {
                    stack.pop();
                },

                Instruction::End => {
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