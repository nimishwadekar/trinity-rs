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
    constants: Vec<u64>,

    // State
    pc: usize,
    stack: Vec<u64>,
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
                iAdd => {
                    let r = self.pop().transmute_to_i64();
                    let l = self.pop().transmute_to_i64();
                    self.push((l + r).transmute_to_u64());
                },

                iSub => {
                    let r = self.pop().transmute_to_i64();
                    let l = self.pop().transmute_to_i64();
                    self.push((l - r).transmute_to_u64());
                },

                iMul => {
                    let r = self.pop().transmute_to_i64();
                    let l = self.pop().transmute_to_i64();
                    self.push((l * r).transmute_to_u64());
                },

                iDiv => {
                    let r = self.pop().transmute_to_i64();
                    let l = self.pop().transmute_to_i64();
                    if r == 0 {
                        return Err(DivideByZero);
                    }
                    self.push((l / r).transmute_to_u64());
                },

                fAdd => {
                    let r = self.pop().transmute_to_f64();
                    let l = self.pop().transmute_to_f64();
                    self.push((l + r).transmute_to_u64());
                },

                fSub => {
                    let r = self.pop().transmute_to_f64();
                    let l = self.pop().transmute_to_f64();
                    self.push((l - r).transmute_to_u64());
                },

                fMul => {
                    let r = self.pop().transmute_to_f64();
                    let l = self.pop().transmute_to_f64();
                    self.push((l * r).transmute_to_u64());
                },

                fDiv => {
                    let r = self.pop().transmute_to_f64();
                    let l = self.pop().transmute_to_f64();
                    self.push((l / r).transmute_to_u64());
                },

                Const { offset } => {
                    self.push(self.get_constant(offset as usize));
                },

                iConst_0 => {
                    self.push(0);
                },

                iConst_1 => {
                    self.push(1);
                },

                Pop => {
                    self.pop();
                },

                iPrint => {
                    println!("{}", self.pop().transmute_to_i64());
                },

                bPrint => {
                    println!("{}", self.pop().transmute_to_bool());
                },

                fPrint => {
                    println!("{}", self.pop().transmute_to_f64());
                },

                nPrint => {
                    assert_eq!(self.pop(), 0);
                    println!("nil");
                },
            };

            self.pc += 1;
        }

        debug!(&self.stack);

        Ok(())
    }

    fn push(&mut self, value: u64) { self.stack.push(value) }
    fn pop(&mut self) -> u64 { self.stack.pop().unwrap() }
    fn get_constant(&self, offset: usize) -> u64 { self.constants[offset] }
}

// Trait for transmute

trait TransmuteToU64 {
    fn transmute_to_u64(&self) -> u64;
}

impl TransmuteToU64 for i64 {
    fn transmute_to_u64(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }
}

impl TransmuteToU64 for bool {
    fn transmute_to_u64(&self) -> u64 {
        if *self { 1 } else { 0 }
    }
}

impl TransmuteToU64 for f64 {
    fn transmute_to_u64(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }
}

trait TransmuteToMany {
    fn transmute_to_i64(&self) -> i64;
    fn transmute_to_bool(&self) -> bool;
    fn transmute_to_f64(&self) -> f64;
}

impl TransmuteToMany for u64 {
    fn transmute_to_i64(&self) -> i64 {
        unsafe { std::mem::transmute(*self) }
    }

    fn transmute_to_bool(&self) -> bool {
        if *self == 1 { true } else { false }
    }

    fn transmute_to_f64(&self) -> f64 {
        unsafe { std::mem::transmute(*self) }
    }
}