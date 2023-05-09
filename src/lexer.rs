use std::{
    rc::Rc,
    str::from_utf8_unchecked,
};

use crate::EOF;

//======================================================================================
//          STRUCTURES
//======================================================================================

#[derive(Debug, Clone)]
pub struct Lexeme {
    src: Rc<String>,
    offset: usize,
    length: usize,
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    IntegerLiteral(i32),

    Plus,

    LParen, RParen,
    LSquare, RSquare,

    Error(char),
}

#[derive(Debug, Clone)]
pub struct Token {
    token: TokenType,
    lexeme: Lexeme,
}

pub struct Lexer {
    src: Rc<String>,
}

pub struct TokenStream<'a> {
    current: Option<Token>,
    offset: usize,
    line: usize,
    line_start_offset: usize,
    lexer: &'a Lexer,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    /// Returns `None` for EOF, `Token::Error` for error token.
    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.clone();
        self.current = self.lex();
        current
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.lexeme.line, self.lexeme.column, self.token)
    }
}


impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::IntegerLiteral(value) => write!(f, "Int {}", value),

            TokenType::Plus => write!(f, "+"),

            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::LSquare => write!(f, "["),
            TokenType::RSquare => write!(f, "]"),

            TokenType::Error(ch) => write!(f, "Error '{}'", ch),
        }
    }
}

impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> TokenStream<'a> {
    pub fn peek(&self) -> Option<Token> {
        self.current.clone()
    }

    fn lex(&mut self) -> Option<Token> {
        self.skip_whitespaces();
        let peek = self.peek_char()?;

        let token = if peek.is_alphabetic() {
            todo!("identifiers");
        }
        else if peek.is_digit(10) {
            self.lex_integer_literal()
        }
        // Special character.
        else {
            self.lex_single_special_character()
        };
        Some(token)
    }

    fn lex_integer_literal(&mut self) -> Token {
        for (length, c) in self.str().char_indices() {
            if !c.is_digit(10) {
                let lexeme = self.lexeme(length);
                self.advance(length);
                let value = lexeme.as_str().parse().unwrap_or_else(|_| unreachable!("lex_integer_literal()"));
                let token = TokenType::IntegerLiteral(value);
                return Token::new(token, lexeme);
            }
        }

        unreachable!("lex_integer_literal()")
    }

    fn lex_single_special_character(&mut self) -> Token {
        let c = self.str().chars().next().expect("lex_single_special_character()");
        let length = c.len_utf8();
        let lexeme = self.lexeme(length);
        self.advance(length);

        let token = match c {
            '+' => TokenType::Plus,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LSquare,
            ']' => TokenType::RSquare,
            c => TokenType::Error(c),
        };
        Token::new(token, lexeme)
    }

    fn skip_whitespaces(&mut self) {
        let mut length = 0;
        let mut line = self.line;
        let mut line_start_offset = self.line_start_offset;

        for (_, c) in self.str().char_indices() {
            if c.is_whitespace() {
                length += 1;
                if c == '\n' {
                    line += 1;
                    line_start_offset = self.offset + length;
                }
            }
            else {
                break;
            }            
        }

        self.advance(length);
        self.line = line;
        self.line_start_offset = line_start_offset;
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
        Lexeme {
            src: self.lexer.src.clone(),
            offset: self.offset,
            length,
            line: self.line + 1,
            column: self.offset - self.line_start_offset + 1,
        }
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
        let mut stream = TokenStream {
            offset: 0,
            lexer: self,
            current: None,
            line: 0,
            line_start_offset: 0,
        };
        stream.current = stream.lex();
        stream
    }

    pub fn print_tokens(&self) {
        for token in self.iter() {
            println!("{}", token);
        }
    }
}

impl Lexeme {
    pub fn as_str(&self) -> &str {
        unsafe { from_utf8_unchecked(&self.src.as_bytes()[self.offset .. self.offset + self.length]) }
    }

    pub fn location(&self) -> String {
        format!("ln {}, col {}", self.line, self.column)
    }
}

impl Token {
    fn new(token: TokenType, lexeme: Lexeme) -> Self {
        Self { token, lexeme }
    }

    pub fn token(&self) -> &TokenType {
        &self.token
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}