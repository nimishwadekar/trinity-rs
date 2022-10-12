use neo_util::{
    debug,
    Instruction::{self, *},
    ExecutableByteCode,
};
use crate::error::RunTimeError::{self, *};

//=========================================
// TYPES
//=========================================

pub struct Vm {
    code: Vec<Instruction>,
    constants: Vec<i32>,

    // State
    pc: usize,
    stack: Vec<i32>,
}

//=========================================
// IMPLEMENTATIONS
//=========================================

impl Vm {
    pub fn execute(code: ExecutableByteCode) -> Result<(), RunTimeError> {
        let ExecutableByteCode {code, constants} = code;
        let mut vm = Vm {
            code,
            constants,
            pc: 0,
            stack: Vec::new(),
        };

        vm.run()
    }

    fn run(&mut self) -> Result<(), RunTimeError> {
        while self.pc < self.code.len() {
            match self.code[self.pc] {
                Add => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l + r);
                },

                Sub => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l - r);
                },

                Mul => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l * r);
                },

                Div => {
                    let r = self.pop();
                    let l = self.pop();
                    if r == 0 {
                        return Err(DivideByZero);
                    }
                    self.push(l / r);
                },

                Load { index } => {
                    self.push(self.get_constant(index as usize));
                },

                Pop => {
                    self.pop();
                }
            };

            self.pc += 1;
        }

        debug!(&self.stack);

        Ok(())
    }

    fn push(&mut self, value: i32) { self.stack.push(value) }
    fn pop(&mut self) -> i32 { self.stack.pop().unwrap() }
    fn get_constant(&self, index: usize) -> i32 { self.constants[index - 1] }
}