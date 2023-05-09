use crate::lexer::{TokenStream, Token, TokenType};

//======================================================================================
//          CONSTANTS
//======================================================================================


//======================================================================================
//          STRUCTURES
//======================================================================================

/*
1	++ --	Suffix/postfix increment and decrement	Left-to-right
    ()	Function call
    []	Array subscripting
    .	Structure and union member access
    ->	Structure and union member access through pointer
    (type){list}	Compound literal(C99)
2	++ --	Prefix increment and decrement	Right-to-left
    + -	Unary plus and minus
    ! ~	Logical NOT and bitwise NOT
    (type)	Cast
    *	Indirection (dereference)
    &	Address-of
    sizeof	Size-of[note 2]
    _Alignof	Alignment requirement(C11)
3	* / %	Multiplication, division, and remainder	Left-to-right
4	+ -	Addition and subtraction
5	<< >>	Bitwise left shift and right shift
6	< <=	For relational operators < and ≤ respectively
    > >=	For relational operators > and ≥ respectively
7	== !=	For relational = and ≠ respectively
8	&	Bitwise AND
9	^	Bitwise XOR (exclusive or)
10	|	Bitwise OR (inclusive or)
11	&&	Logical AND
12	||	Logical OR
13	?:	Ternary conditional[note 3]	Right-to-left
14	=	Simple assignment
    += -=	Assignment by sum and difference
    *= /= %=	Assignment by product, quotient, and remainder
    <<= >>=	Assignment by bitwise left shift and right shift
    &= ^= |=	Assignment by bitwise AND, XOR, and OR
15	,	Comma	Left-to-right
*/

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
enum OperatorPrecedence {
    None,
    Add,
    Unary,
    Call,
}

#[derive(Debug, Clone, Copy)]
enum ExprOperator {
    Add,
    UnaryPlus,
    Subscript,
}

#[derive(Debug)]
enum Expr {
    ExprOperation{ operator: ExprOperator, operands: Vec<Box<Expr>> },
    IntegerLiteral(i32),
}

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream<'a>) -> Self {
        Self {
            tokens,
        }
    }

    pub fn parse(&mut self) {
        let expr = self.parse_program();
        match expr {
            Ok(expr) => println!("{}", expr),
            Err(e) => println!("{}", e),
        }
    }

    fn parse_program(&mut self) -> Result<Box<Expr>, String> {
        match self.peek_token()?.ok_or("Temp error: EOP".to_string())? {
            t if t.token().is_expr_starter() => self.parse_expr(),
            _ => Err("non-expression-starter".to_string()),
        }
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>, String> {
        self.parse_expr_recursive(OperatorPrecedence::None as u8)
    }
}

//=======================================
//          EXPR PARSING METHODS
//=======================================

impl<'a> Parser<'a> {
    fn parse_expr_recursive(&mut self, min_bp: u8) -> Result<Box<Expr>, String> {
        // Assumes first token of expression has been validated.
        let tok = self.expect_token()?;
        let mut lhs = match tok.token() {
            TokenType::IntegerLiteral(value) => Box::new(Expr::IntegerLiteral(*value)),
            TokenType::LParen => {
                let lhs = self.parse_expr_recursive(OperatorPrecedence::None as u8)?;
                self.consume_token(TokenType::RParen)?;
                lhs
            },
            t if t.as_prefix_operator().is_some() => {
                let op = unsafe { t.as_prefix_operator().unwrap_unchecked() };
                let ((), rbp) = op.prefix_binding_power();
                let rhs = self.parse_expr_recursive(rbp)?;
                Box::new(Expr::ExprOperation { operator: op, operands: vec![rhs] })
            },

            _ => return Err(err_format("Expected expression", tok)),
        };

        loop {
            let op = match self.peek_token()? {
                None => break,
                Some(t) => match t.token() {
                    TokenType::Plus => ExprOperator::Add,
                    TokenType::LSquare => ExprOperator::Subscript,
                    _ => break,
                },
            };

            if let Some((lbp, ())) = op.postfix_binding_power() {
                if lbp < min_bp {
                    break;
                }
                self.next_token().unwrap_or_else(|_| unreachable!());

                lhs = if let ExprOperator::Subscript = op {
                    let rhs = self.parse_expr_recursive(0)?;
                    self.consume_token(TokenType::RSquare)?;
                    Box::new(Expr::ExprOperation { operator: op, operands: vec![lhs, rhs] })
                } else {
                    Box::new(Expr::ExprOperation { operator: op, operands: vec![lhs] })
                };
                continue;
            }

            if let Some((lbp, rbp)) = op.infix_binding_power() {
                if lbp < min_bp {
                    break;
                }
                self.next_token().unwrap_or_else(|_| unreachable!());
    
                let rhs = self.parse_expr_recursive(rbp)?;
                lhs = Box::new(Expr::ExprOperation { operator: op, operands: vec![lhs, rhs] });
                continue;
            }
            
            break;
        }

        Ok(lhs)
    }
}

impl ExprOperator {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            ExprOperator::UnaryPlus => ((), OperatorPrecedence::Unary.bp().1),
            op => unreachable!("prefix_binding_power(): {:?}", op),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            ExprOperator::Add => Some(OperatorPrecedence::Add.bp()),
            _ => None,
            //op => unreachable!("infix_binding_power(): {:?}", op),
        }
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        match self {
            ExprOperator::Subscript => Some((OperatorPrecedence::Call.bp().0, ())),
            _ => None,
            //op => unreachable!("postfix_binding_power(): {:?}", op),
        }
    }
}

impl OperatorPrecedence {
    fn bp(&self) -> (u8, u8) {
        match self {
            // TODO: Right associative precedences

            _ => ((*self as u8) * 2, (*self as u8) * 2 + 1),
        }
    }
}

impl TokenType {
    fn is_expr_starter(&self) -> bool {
        match self {
            TokenType::IntegerLiteral(..)
            | TokenType::Plus
            | TokenType::LParen
            => true,

            _ => false,
        }
    }

    fn as_prefix_operator(&self) -> Option<ExprOperator> {
        match self {
            TokenType::Plus => Some(ExprOperator::UnaryPlus),
            _ => None,
        }
    }
}

//=======================================
//          HELPER METHODS
//=======================================

impl<'a> Parser<'a> {
    #[inline]
    fn peek_token(&mut self) -> Result<Option<Token>, String> {
        match self.tokens.peek() {
            None => Ok(None),
            Some(t) => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
                _ => Ok(Some(t)),
            },
        }
    }

    #[inline]
    fn next_token(&mut self) -> Result<Option<Token>, String> {
        match self.tokens.next() {
            None => Ok(None),
            Some(t) => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
                _ => Ok(Some(t)),
            },
        }
    }

    /// Returns an `Err` if `EOF` was encountered.
    #[inline]
    fn expect_token(&mut self) -> Result<Token, String> {
        match self.tokens.next().ok_or("Unexpected EOF".to_string())? {
            t => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
                _ => Ok(t),
            },
        }
    }

    #[inline]
    fn consume_token(&mut self, expected: TokenType) -> Result<(), String> {
        let t = self.tokens.next().ok_or("Unexpected EOF".to_string())?;
        match t.token() {
            TokenType::Error(..) => Err(err_format("Unknown character", t)),
            t if t == &expected => Ok(()),
            t => Err(format!("Expected `{}`, found `{}`", expected, t)),
        }
    }
}

//=======================================
//          FORMAT METHODS
//=======================================

impl Expr {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        const NEXT_INDENT: usize = 3;
        write!(f, "{:indent$}", "")?;
        match self {
            Expr::IntegerLiteral(value) => writeln!(f, "Integer {}", value),
            Expr::ExprOperation { operator, operands } => {
                writeln!(f, "{:?}", operator)?;
                for operand in operands {
                    operand.display_format(f, indent + NEXT_INDENT)?;
                };
                Ok(())
            },
        }
    }
}

#[inline(always)]
fn err_format(err: &str, t: Token) -> String {
    format!("<{}> `{}`: {}", t.lexeme().location(), t.lexeme(), err)
}