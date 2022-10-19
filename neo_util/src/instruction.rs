use std::fmt::Debug;

/// Prefixes:
/// i - int
/// b - bool
/// f - float
/// n - nil
/// 
/// In the documentation regarding stack manipulation, the stack increases rightwards.
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub enum Instruction {
    // Operations

    /// a, b -> result
    iAdd,
    /// a, b -> result
    iSub,
    /// a, b -> result
    iMul,
    /// a, b -> result
    iDiv,

    /// a, b -> result
    fAdd,
    /// a, b -> result
    fSub,
    /// a, b -> result
    fMul,
    /// a, b -> result
    fDiv,

    // Global Data

    /// Stores the value at the top of the stack into the global data at `offset`.
    /// 
    /// value -> .
    StoreGlobal { offset: u8 },

    /// Loads the value of the global data at `offset` onto the stack.
    /// 
    /// . -> value
    LoadGlobal { offset: u8 },

    // Stack Manipulation
    /// Loads the constant at `offset` in the constant pool.
    /// 
    /// . -> constant
    Const { offset: u8 },

    /// Loads the integer `0` onto the stack. Also used to represent `false` and `nil`.
    /// 
    /// . -> 0
    iConst_0,

    /// Loads the integer `1` onto the stack. Also used to represent `true`.
    /// 
    /// . -> 1
    iConst_1,

    /// Pops and discards the top element from the stack.
    /// 
    /// value -> .
    Pop,

    // IO

    /// value -> .
    iPrint,
    /// value -> .
    bPrint,
    /// value -> .
    fPrint,
    /// value -> .
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

            StoreGlobal { offset } => write!(f, "StoreGlobal {:?}", offset),
            LoadGlobal { offset } => write!(f, "LoadGlobal {:?}", offset),

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