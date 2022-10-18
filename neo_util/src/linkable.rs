use std::fmt::Debug;

use crate::Instruction;

pub struct LinkableByteCode {
    /// Code segment.
    pub code: Vec<Instruction>,

    /// Constant pool.
    pub constants: Vec<u64>,
}

impl Debug for LinkableByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "ByteCode:\nCode:")?;
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
        }
    }

    pub fn push_code(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    /// Returns the offset the data is at.
    pub fn push_constant(&mut self, data: u64) -> usize {
        let offset = self.constants.len();
        self.constants.push(data);
        offset
    }
}