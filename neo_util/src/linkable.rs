use std::fmt::Debug;

use crate::Instruction;

pub struct LinkableByteCode {
    /// Code segment.
    pub code: Vec<Instruction>,

    /// Constant pool.
    pub constants: Vec<u64>,

    /// Code related to the initialization of global data.
    pub data_init_code: Vec<Instruction>,

    pub data_count: usize,

    is_currently_data_init_code: bool,
}

impl Debug for LinkableByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "ByteCode:")?;
        writeln!(f, "Data-Init Code:")?;
        for (i, instruction) in self.data_init_code.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", instruction)?;
        }
        writeln!(f, "\nCode:")?;
        for (i, instruction) in self.code.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", instruction)?;
        }
        writeln!(f, "\nConstants:")?;
        for (i, &constant) in self.constants.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", constant)?;
        }
        Ok(())
    }
}

impl LinkableByteCode {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            data_init_code: Vec::new(),

            is_currently_data_init_code: false,
            data_count: 0,
        }
    }

    pub fn push_code(&mut self, instr: Instruction) {
        if self.is_currently_data_init_code { self.data_init_code.push(instr) }
        else { self.code.push(instr) }
    }

    /// Returns the offset the constant is at.
    pub fn push_constant(&mut self, constant: u64) -> usize {
        let offset = self.constants.len();
        self.constants.push(constant);
        offset
    }

    /// Returns the offset of the new global variable.
    pub fn begin_data_init_code(&mut self) -> usize {
        self.is_currently_data_init_code = true;
        self.data_count += 1;
        self.data_count - 1
    }
    pub fn end_data_init_code(&mut self) { self.is_currently_data_init_code = false; }
}