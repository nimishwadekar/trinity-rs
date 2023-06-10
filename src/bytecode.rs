use std::fmt::Display;

use crate::CompilerResult;

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================

#[derive(Debug)]
pub enum Instruction {
    /// Push `constants[index]`.
    LoadConstantInt { index: u8 },

    /// Push `0`.
    LoadConstantZeroInt,

    /// Push `constants[index]`.
    LoadConstantFloat { index: u8 },

    /// Push `0.0`.
    LoadConstantZeroFloat,

    /// Push [bool] value.
    LoadConstantBool(bool),

    LoadUnit,

    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MulInt,
    MulFloat,
    DivInt,
    DivFloat,
    ModInt,

    IsZeroInt,
    IsZeroFloat,
    IsPositiveInt,
    IsPositiveFloat,
    IsPositiveOrZeroInt,
    IsPositiveOrZeroFloat,
    IsEqualBool,

    NotBool,
    AndBool,
    OrBool,

    LoadVariable { index: u8 },
    StoreVariable { index: u8 },

    PrintInt,
    PrintFloat,
    PrintBool,

    Pop,

    End,
}

#[derive(Debug)]
pub struct Constants<T: Display> {
    pool: Vec<T>
}

#[derive(Debug)]
pub struct ByteCode {
    pub code: Vec<Instruction>,

    /// constant -> index.
    pub constants_int: Constants<i64>,
    pub constants_float: Constants<f64>,
    pub max_identifiers_in_scope: usize,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl<T: Display> std::ops::Deref for Constants<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.pool.as_slice()
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadConstantInt { index } => write!(f, "LoadConstantInt {index}"),
            Instruction::LoadConstantFloat { index } => write!(f, "LoadConstantFloat {index}"),
            Instruction::LoadConstantBool(value) => write!(f, "LoadConstantBool {value}"),
            Instruction::LoadVariable { index } => write!(f, "LoadVariable {index}"),
            Instruction::StoreVariable { index } => write!(f, "StoreVariable {index}"),
            i => write!(f, "{:?}", i),
        }
    }
}

impl<T: Display> std::fmt::Display for Constants<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut constants = self.pool.iter().enumerate().peekable();
        while let Some((index, constant)) = constants.next() {
            write!(f, "[{}] {}", index, constant)?;
            if constants.peek().is_some() {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "CONSTANTS INT:\n{}\n\nCONSTANTS FLOAT:\n{}\n\nCODE:", self.constants_int, self.constants_float)?;
        let mut code = self.code.iter().peekable();
        while let Some(instr) = code.next() {
            write!(f, "{}", instr)?;
            if code.peek().is_some() {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl ByteCode {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants_int: Constants::new(),
            constants_float: Constants::new(),
            max_identifiers_in_scope: 0,
        }
    }

    pub fn write_instruction(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    pub fn insert_constant_int(&mut self, constant: i64) -> Result<u8, String> {
        self.constants_int.insert(constant)
    }

    pub fn insert_constant_float(&mut self, constant: f64) -> Result<u8, String> {
        self.constants_float.insert(constant)
    }
}

impl<T: Display> Constants<T> {
    fn new() -> Self {
        Self {
            pool: Vec::new(),
        }
    }

    fn insert(&mut self, constant: T) -> CompilerResult<u8> {
        if self.pool.len() <= u8::MAX as usize {
            self.pool.push(constant);
            Ok((self.pool.len() - 1) as u8)
        }
        else {
            Err("Too many constants".to_string())
        }
    }
}