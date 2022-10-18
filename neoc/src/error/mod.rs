mod parse;
mod compile;

pub use parse::{ParseError, parse_errors};
pub use compile::CompileError;

#[derive(Debug)]
pub enum NeoCError{
    Parse(ParseError),
    Compile(CompileError),
}