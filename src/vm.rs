use crate::bytecode::{ByteCode, Instruction};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



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
    pub fn execute(code: ByteCode, trace: bool) -> Result<(), String> {
        let ByteCode { code, constants } = code;
        let mut pc = 0;
        let mut stack = Vec::new();

        if trace {
            println!("CONSTANTS:\n{}\n", constants);
        }

        let constants = constants.to_vec();

        loop {
            if trace {
                println!("Stack: {:?}\nNext: {}", stack, code[pc]);
            }

            match code[pc] {
                Instruction::LoadConstant { index } => {
                    stack.push(constants[index as usize]);
                },

                Instruction::Add => {
                    let r = stack.pop().unwrap();
                    let l = stack.pop().unwrap();
                    stack.push(l + r);
                },

                Instruction::Print => {
                    if trace {
                        print!(">>> ");
                    }
                    println!("{}", stack.pop().unwrap());
                },

                Instruction::Pop => {
                    stack.pop().unwrap();
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