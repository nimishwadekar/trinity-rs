//=========================================
// TYPES
//=========================================

#[derive(Debug)]
pub enum CompileError {
    TypeMismatch(String),

    TooManyConstants,
}