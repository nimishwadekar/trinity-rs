use std::fmt::Debug;

/// Prefixes:
/// i - int
/// b - bool
/// f - float
/// n - nil
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub enum Instruction {
    // Operations
    iAdd,
    iSub,
    iMul,
    iDiv,

    fAdd,
    fSub,
    fMul,
    fDiv,

    // Stack Manipulation
    /// Loads the constant at `offset` in the constant pool.
    Const { offset: u8 },
    /// Loads the integer `0` onto the stack. Also used to represent `false` and `nil`.
    iConst_0,
    /// Loads the integer `1` onto the stack. Also used to represent `true`.
    iConst_1,
    /// Pops and discards the top element from the stack.
    Pop,

    // IO
    iPrint,
    bPrint,
    fPrint,
    nPrint,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use Instruction::*;
        match self {
            iAdd => write!(f, "iAdd"),
            iSub => write!(f, "iSub"),
            iMul => write!(f, "iMul"),
            iDiv => write!(f, "iDiv"),

            fAdd => write!(f, "fAdd"),
            fSub => write!(f, "fSub"),
            fMul => write!(f, "fMul"),
            fDiv => write!(f, "fDiv"),

            Const { offset } => write!(f, "Const {:?}", offset),
            iConst_0 => write!(f, "iConst_0"),
            iConst_1 => write!(f, "iConst_1"),
            Pop => write!(f, "Pop"),

            iPrint => write!(f, "iPrint"),
            bPrint => write!(f, "bPrint"),
            fPrint => write!(f, "fPrint"),
            nPrint => write!(f, "nPrint"),
        }
    }
}