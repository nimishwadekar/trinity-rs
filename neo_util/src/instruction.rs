use std::fmt::Debug;

pub enum Instruction {
    // Operations
    Add,
    Sub,
    Mul,
    Div,

    // Stack Manipulation
    /// Loads the constant at `index` in the constant pool.
    Load { index: u8 },
    Pop,

    // IO
    Print,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use Instruction::*;
        match self {
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Mul => write!(f, "mul"),
            Div => write!(f, "div"),

            Load { index } => write!(f, "load {:?}", index),
            Pop => write!(f, "pop"),

            Print => write!(f, "print"),
        }
    }
}