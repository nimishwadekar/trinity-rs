//=========================================
// TYPES
//=========================================

#[derive(Debug)]
pub enum CompileError {
    TypeMismatch(String),
    VariableRedeclared(String),
    UndefinedVariable(String),

    TooManyConstants,
    TooManyGlobals,
}