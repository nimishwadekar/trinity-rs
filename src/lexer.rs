use std::{
    rc::Rc,
    str::from_utf8_unchecked,
};

use crate::EOF;

//======================================================================================
//          STRUCTURES
//======================================================================================

pub struct Lexeme {
    src: Rc<String>,
    offset: usize,
    length: usize,
}

#[derive(Debug)]
pub enum Token {
    IntegerLiteral{ lexeme: Lexeme },

    Plus,

    Error,
}

pub struct Lexer {
    src: Rc<String>,
}

pub struct TokenStream<'a> {
    offset: usize,
    lexer: &'a Lexer,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    /// Returns `None` for EOF, `Token::Error` for error token.
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespaces();
        let peek = self.peek_char()?;

        let token = if peek.is_alphabetic() {
            todo!("identifiers");
        }
        else if peek.is_digit(10) {
            self.lex_integer_literal()
        }
        // Special character.
        else if !peek.is_alphanumeric() {
            self.lex_single_special_character()
        }
        else {
            Token::Error
        };

        Some(token)
    }
}

impl std::fmt::Debug for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", unsafe { from_utf8_unchecked(&self.src.as_bytes()[self.offset .. self.offset + self.length]) })
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> TokenStream<'a> {
    fn lex_integer_literal(&mut self) -> Token {
        for (length, c) in self.str().char_indices() {
            if !c.is_digit(10) {
                let lexeme = self.lexeme(length);
                self.advance(length);
                return Token::IntegerLiteral { lexeme }
            }
        }

        unreachable!("lex_integer_literal()")
    }

    fn lex_single_special_character(&mut self) -> Token {
        let c = self.str().chars().next().expect("lex_single_special_character()");
        self.advance(c.len_utf8());

        match c {
            '+' => Token::Plus,
            _ => Token::Error,
        }
    }

    fn skip_whitespaces(&mut self) {
        for (length, c) in self.str().char_indices() {
            if !c.is_whitespace() {
                self.advance(length);
                break;
            }
        }
    }

    /// Return `None` if EOF.
    fn peek_char(&self) -> Option<char> {
        match self.str().chars().next()? {
            EOF => None,
            c => Some(c),
        }
    }

    fn lexeme(&self, length: usize) -> Lexeme {
        assert!(self.offset + length <= self.lexer.src.len());
        Lexeme { src: self.lexer.src.clone(), offset: self.offset, length }
    }

    fn advance(&mut self, length: usize) {
        self.offset += length;
    }

    /// Returns the remaining string slice.
    fn str(&self) -> &str {
        unsafe { from_utf8_unchecked(&self.lexer.src.as_bytes()[self.offset..]) }
    }
}

impl Lexer {
    pub fn new(src: Rc<String>) -> Self {
        Self { src }
    }

    pub fn iter(&self) -> TokenStream {
        TokenStream {
            offset: 0,
            lexer: self,
        }
    }
}