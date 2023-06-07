use crate::{lexer::{TokenStream, Token, TokenType, Lexeme}, CompilerResult, err};

use super::ast::{Expr, ExprType, Stmt, StmtType, ParseTree, DataType};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================

macro_rules! span_lexeme {
    ($first:expr, $last:expr) => {
        $first.lexeme().clone() + $last.lexeme().clone()
    };
}

macro_rules! stmt {
    ($s:expr, $lexeme:expr) => {
        Box::new(Stmt::new($s, $lexeme))
    };
}

macro_rules! untyped_expr {
    ($e:expr, $lexeme:expr) => {
        Box::new(Expr::new($e, DataType::Unit, $lexeme))
    };
}

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

pub struct Constructor<'a> {
    tokens: TokenStream<'a>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> Constructor<'a> {
    pub fn parse(tokens: TokenStream<'a>)-> CompilerResult<ParseTree> {
        let mut parser = Self { tokens };
        Ok(ParseTree::new(parser.parse_program()?))
    }

    fn parse_program(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut stmts = Vec::new();
        loop {
            let stmt = match self.peek_token()? {
                None => break,
                Some(token) => match token.token() {
                    // Print statement.
                    TokenType::Print => self.parse_print_stmt()?,

                    // Expression statement.
                    t if t.is_expr_starter() => self.parse_expr_stmt()?,

                    // Empty statement.
                    TokenType::Semicolon => {
                        self.next_token().unwrap();
                        continue;
                    },

                    _ => return err!("Invalid start of statement", token.lexeme()),
                }
            };
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    /// PrintStmt := `print` Expr `;`
    fn parse_print_stmt(&mut self) -> CompilerResult<Box<Stmt>> {
        let first_token = self.consume_token(TokenType::Print)?;
        let expr = self.parse_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(first_token, last_token);
        Ok(stmt!(StmtType::Print(expr), lexeme))
    }

    /// ExprStmt := Expr `;`
    fn parse_expr_stmt(&mut self) -> CompilerResult<Box<Stmt>> {
        let expr = self.parse_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(expr, last_token);
        Ok(stmt!(StmtType::Expr(expr), lexeme))
    }

    /// 
    /// Expr := Expr (`+`) Expr \
    /// Expr := (`+`) Expr \
    /// Expr := `(` Expr `)`
    /// 
    fn parse_expr(&mut self) -> CompilerResult<Box<Expr>> {
        self.parse_expr_recursive(OperatorPrecedence::None as u8)
    }
}

//=======================================
//          EXPR PARSING METHODS
//=======================================

impl<'a> Constructor<'a> {
    fn parse_expr_recursive(&mut self, min_bp: u8) -> CompilerResult<Box<Expr>> {
        // Assumes first token of expression has been validated.
        let tok = self.expect_token()?;
        let mut lhs = match tok.token() {
            TokenType::IntegerLiteral => untyped_expr!(ExprType::from_integer_literal(tok.lexeme())?, tok.lexeme().clone()),

            TokenType::FloatLiteral => untyped_expr!(ExprType::from_float_literal(tok.lexeme())?, tok.lexeme().clone()),

            TokenType::LParen => {
                let lhs = self.parse_expr_recursive(OperatorPrecedence::None as u8)?;
                let token_rparen = self.consume_token(TokenType::RParen)?;

                let lexeme = span_lexeme!(tok, token_rparen);
                untyped_expr!(lhs.take_expr(), lexeme)
            },

            t if t.as_prefix_operator().is_some() => {
                let op = unsafe { t.as_prefix_operator().unwrap_unchecked() };
                let ((), rbp) = op.prefix_binding_power();
                let rhs = self.parse_expr_recursive(rbp)?;

                let lexeme = span_lexeme!(tok, rhs);
                let expr = ExprType::new(op, vec![rhs]);
                untyped_expr!(expr, lexeme)
            },

            _ => return err!("Expected expression", tok.lexeme()),
        };

        loop {
            let (op, token_op) = match self.peek_token()? {
                None => break,
                Some(t) => (match t.token() {
                    TokenType::Plus => ExprOperator::Add,
                    TokenType::LSquare => ExprOperator::Subscript,
                    _ => break,
                }, t),
            };

            if let Some((lbp, ())) = op.postfix_binding_power() {
                if lbp < min_bp {
                    break;
                }
                self.next_token().unwrap();

                lhs = if let ExprOperator::Subscript = op {
                    let rhs = self.parse_expr_recursive(0)?;
                    let token_rsquare = self.consume_token(TokenType::RSquare)?;

                    let lexeme = span_lexeme!(lhs, token_rsquare);
                    let expr = ExprType::new(op, vec![lhs, rhs]);
                    untyped_expr!(expr, lexeme)
                } else {
                    let lexeme = span_lexeme!(lhs, token_op);
                    let expr = ExprType::new(op, vec![lhs]);
                    untyped_expr!(expr, lexeme)
                };
                continue;
            }

            if let Some((lbp, rbp)) = op.infix_binding_power() {
                if lbp < min_bp {
                    break;
                }
                self.next_token().unwrap();

                let rhs = self.parse_expr_recursive(rbp)?;

                let lexeme = span_lexeme!(lhs, rhs);
                let expr = ExprType::new(op, vec![lhs, rhs]);
                lhs = untyped_expr!(expr, lexeme);
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
            TokenType::IntegerLiteral
            | TokenType::FloatLiteral
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

impl ExprType {
    fn new(operator: ExprOperator, operands: Vec<Box<Expr>>) -> Self {
        let operand_count = operands.len();
        let mut operands = operands.into_iter();
        let msg = format!("`{operator:?}` with {operand_count} operand{}", if operand_count == 1 {""} else {"s"});

        match operator {
            // Unary
            ExprOperator::UnaryPlus => {
                assert_eq!(operand_count, 1, "{}", msg);
                let expr = unsafe { operands.next().unwrap_unchecked() };
                ExprType::Positive(expr)
            },

            // Binary
            ExprOperator::Add => {
                assert_eq!(operand_count, 2, "{}", msg);
                let l = unsafe { operands.next().unwrap_unchecked() };
                let r = unsafe { operands.next().unwrap_unchecked() };
                ExprType::Add{ l, r }
            },
            ExprOperator::Subscript => {
                assert_eq!(operand_count, 2, "{}", msg);
                todo!("subscript expression")
            },
        }
    }

    fn from_integer_literal(lexeme: &Lexeme) -> CompilerResult<Self> {
        match lexeme.parse::<i64>() {
            Ok(val) => Ok(Self::IntegerLiteral(val)),
            Err(e) => err!(e.to_string().as_str(), lexeme),
        }
    }

    fn from_float_literal(lexeme: &Lexeme) -> CompilerResult<Self> {
        match lexeme.parse::<f64>() {
            Ok(val) => Ok(Self::FloatLiteral(val)),
            Err(e) => err!(e.to_string().as_str(), lexeme),
        }
    }
}

//=======================================
//          HELPER METHODS
//=======================================

impl<'a> Constructor<'a> {
    #[inline]
    fn peek_token(&mut self) -> CompilerResult<Option<Token>> {
        match self.tokens.peek() {
            None => Ok(None),
            Some(t) => match t.token() {
                TokenType::Error(..) => err!("Unknown character", t.lexeme()),
                _ => Ok(Some(t)),
            },
        }
    }

    #[inline]
    fn next_token(&mut self) -> CompilerResult<Option<Token>> {
        match self.tokens.next() {
            None => Ok(None),
            Some(t) => match t.token() {
                TokenType::Error(..) => err!("Unknown character", t.lexeme()),
                _ => Ok(Some(t)),
            },
        }
    }

    /// Returns an `Err` if `EOF` was encountered.
    #[inline]
    fn expect_token(&mut self) -> CompilerResult<Token> {
        let t = self.tokens.next();
        match self.handle_eof_token(t)? {
            t => match t.token() {
                TokenType::Error(..) => err!("Unknown character", t.lexeme()),
                _ => Ok(t),
            },
        }
    }

    #[inline]
    fn consume_token(&mut self, expected: TokenType) -> CompilerResult<Token> {
        let location = self.tokens.location();
        let token = match self.tokens.next() {
            None => return Err(format!("<{}> Expected `{}`, reached end of file", location, expected)),
            Some(t) => t,
        };
        match token.token() {
            TokenType::Error(..) => err!("Unknown character", token.lexeme()),
            t if t == &expected => Ok(token),
            t => Err(format!("<{}> Expected `{}`, found `{}`", token.lexeme().location(), expected, t)),
        }
    }

    #[inline(always)]
    fn handle_eof_token(&self, token: Option<Token>) -> CompilerResult<Token> {
        token.ok_or(format!("<{}> Unexpected end of file", self.tokens.location()))
    }
}