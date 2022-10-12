use crate::error::RunTimeError;
use neo_util::ExecutableByteCode;
use vm::Vm;

mod error;
mod vm;

pub fn execute(code: ExecutableByteCode) -> Result<(), RunTimeError> {
    Vm::execute(code)
}
