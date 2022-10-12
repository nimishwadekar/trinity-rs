#[derive(Debug)]
pub enum CompilationError {
    InvalidToken,
    UnexpectedEOF,
    UnrecognizedToken,
}