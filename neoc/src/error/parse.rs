use std::fmt::Debug;

use lalrpop_util::{
    self as lalrpop,
    ErrorRecovery,
    lexer::Token,
};

use crate::NeoCError;

//=========================================
// TYPES
//=========================================

pub struct Location {
    line: usize,
    col: usize,
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken { location: Location },
    UnexpectedEOF,
    UnrecognizedToken { location: Location, lexeme: String },
    ExtraToken { location: Location, lexeme: String },
}

//=========================================
// TRAIT IMPLEMENTATIONS
//=========================================

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Line {:?}, Col {:?}", self.line, self.col)
    }
}

//=========================================
// IMPLEMENTATIONS
//=========================================

impl Location {
    // TODO: Cache newline locations for faster lookup.
    pub fn from(source: &str, byte_offset: usize) -> Option<Self> {
        if byte_offset == 0 {
            return Some(Self { line: 1, col: 1 });
        }

        // Check for newline before `byte_offset`.
        match source[byte_offset - 1..].chars().next() {
            Some(c) => {
                let (line, text) = source[0..byte_offset].lines().enumerate().last()?;
                match c {
                    '\n' | '\r' => Some(Self{
                        line: line + 2,
                        col: 1
                    }),
                    _ => Some(Self{
                        line: line + 1,
                        col: text.chars().count() + 1
                    }),
                }
            },
            None => unreachable!("There should always be at least one character in the slice because `byte_offset` is greater than zero."),
        }
    }
}

impl ParseError {
    pub fn from(e: lalrpop::ParseError<usize, Token, ParseError>, source: &str) -> Self {
        match e {
            lalrpop::ParseError::InvalidToken { location } => Self::InvalidToken {
                location: Location::from(source, location).expect("Location parsing failed"),
            },

            lalrpop::ParseError::UnrecognizedEOF { .. } => Self::UnexpectedEOF,

            lalrpop::ParseError::UnrecognizedToken { token, .. } => Self::UnrecognizedToken {
                location: Location::from(source, token.0).expect("Location parsing failed"),
                lexeme: String::from(token.1.1)
            },

            lalrpop::ParseError::ExtraToken { token } => Self::ExtraToken {
                location: Location::from(source, token.0).expect("Location parsing failed"),
                lexeme: String::from(token.1.1)
            },

            lalrpop::ParseError::User { error } => error,
        }
    }
}

//=========================================
// GENERAL FUNCTIONS
//=========================================

pub fn parse_errors(errors: Vec<ErrorRecovery<usize, Token, ParseError>>, source: &str) -> Vec<NeoCError> {
    let mut compilation_errors = Vec::new();
    for e in errors {
        compilation_errors.push(NeoCError::Parse(ParseError::from(e.error, source)));
    }
    compilation_errors
}