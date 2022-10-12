use std::fmt::Debug;

use crate::Instruction;

pub struct ExecutableByteCode {
    pub code: Vec<Instruction>,
    pub constants: Vec<i32>,
}

impl Debug for ExecutableByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "ByteCode {{\nCode:")?;
        for (i, instruction) in self.code.iter().enumerate() {
            writeln!(f, "\t{i}\t{:?}", instruction)?;
        }
        writeln!(f, "\nConstants:")?;
        for (i, &constant) in self.constants.iter().enumerate() {
            writeln!(f, "\t{}\t{:?}", i + 1, constant)?;
        }
        Ok(())
    }
}