use std::collections::{HashMap, hash_map::Entry};

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
    LoadConstant{ index: u8 },
    Add,
    Pop,
    Print,

    End,
}

#[derive(Debug)]
pub struct Constants {
    pool: HashMap<i64, u8>,
}

#[derive(Debug)]
pub struct ByteCode {
    pub code: Vec<Instruction>,

    /// constant -> index.
    pub constants: Constants,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadConstant { index } => write!(f, "LoadConstant {}", index),
            Instruction::Add => write!(f, "Add"),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::Print => write!(f, "Print"),
            Instruction::End => write!(f, "End"),
        }
    }
}

impl std::fmt::Display for Constants {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut constants = self.to_vec().into_iter().enumerate().peekable();
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
        writeln!(f, "CONSTANTS:\n{}\n\nCODE:", self.constants)?;
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
            constants: Constants::new(),
        }
    }

    pub fn write_instruction(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    pub fn add_constant(&mut self, constant: i64) -> Result<u8, String> {
        self.constants.add_constant(constant)
    }
}

impl Constants {
    fn new() -> Self {
        Self {
            pool: HashMap::new(),
        }
    }

    pub fn to_vec(&self) -> Vec<i64> {
        let mut vec = vec![0; self.pool.len()];
        for (&constant, &index) in self.pool.iter() {
            vec[index as usize] = constant;
        }
        vec
    }

    fn add_constant(&mut self, constant: i64) -> CompilerResult<u8> {
        let constant_count = self.pool.len();
        match self.pool.entry(constant) {
            Entry::Occupied(e) => Ok(*e.get()),
            Entry::Vacant(e) => {
                if constant_count <= u8::MAX as usize {
                    Ok(*e.insert(constant_count as u8))
                }
                else {
                    Err("Too many constants".to_string())
                }
            }
        }
    }
}