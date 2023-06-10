use crate::{lexer::{Token, TokenType, Lexeme}, CompilerResult, err};

use super::{ast::{Expr, ExprType, Stmt, StmtType, ParseTree, DataType}, Parser};

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
    Or,
    And,
    Equal,
    LesserGreater,
    Add,
    Multiply,
    Unary,
    Call,
}

#[derive(Debug, Clone, Copy)]
enum ExprOperator {
    // Binary
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,

    Equal,
    NotEqual,
    Lesser,
    LesserEqual,
    Greater,
    GreaterEqual,

    And,
    Or,

    Subscript,

    // Unary
    UnaryPlus,
    UnaryMinus,
    Not,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================



//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> Parser<'a> {
    pub fn construct(&mut self)-> CompilerResult<ParseTree> {
        Ok(ParseTree::new(self.construct_program()?))
    }

    fn construct_program(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut stmts = Vec::new();
        loop {
            let stmt = match self.peek_token()? {
                None => break,
                Some(token) => match token.token() {
                    // Temp
                    // Print statement.
                    TokenType::Print => self.construct_print_stmt()?,

                    // Let statement.
                    TokenType::Let => self.construct_let_stmt()?,

                    // Empty statement.
                    TokenType::Semicolon => {
                        self.next_token().unwrap();
                        continue;
                    },

                    // Expression statement.
                    t if t.is_expr_starter() => self.construct_expr_stmt()?,

                    _ => return err!("Invalid start of statement", token.lexeme()),
                }
            };
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn try_construct_stmt(&mut self) -> Option<CompilerResult<Box<Stmt>>> {
        match match self.peek_token() {
            Err(e) => return Some(Err(e)),
            Ok(tok) => tok,
        }?.token() {
            // Temp
            // Print statement.
            TokenType::Print => Some(self.construct_print_stmt()),

            // Let statement.
            TokenType::Let => Some(self.construct_let_stmt()),

            // Empty statement.
            TokenType::Semicolon => {
                while let TokenType::Semicolon = match self.peek_token() {
                    Err(e) => return Some(Err(e)),
                    Ok(tok) => tok,
                }?.token() {
                    self.next_token().unwrap();
                }
                self.try_construct_stmt()
            },

            // Not a statement starter (except expression statements).
            _ => None,
        }
    }

    /// LetStmt := `let` Identifier `:` Identifier `=` Expr `;`
    fn construct_let_stmt(&mut self) -> CompilerResult<Box<Stmt>> {
        let first_token = self.consume_token(TokenType::Let)?;
        let identifier = self.consume_token(TokenType::Identifier)?;
        self.consume_token(TokenType::Colon)?;
        let dtype = self.construct_dtype()?;
        self.consume_token(TokenType::Equal)?;
        let initialiser = self.construct_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let identifier = identifier.lexeme().clone();
        let lexeme = span_lexeme!(first_token, last_token);
        Ok(stmt!(StmtType::Let { identifier, dtype, initialiser }, lexeme))
    }

    /// PrintStmt := `print` Expr `;`
    fn construct_print_stmt(&mut self) -> CompilerResult<Box<Stmt>> {
        let first_token = self.consume_token(TokenType::Print)?;
        let expr = self.construct_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(first_token, last_token);
        Ok(stmt!(StmtType::Print(expr), lexeme))
    }

    /// ExprStmt := Expr `;`
    fn construct_expr_stmt(&mut self) -> CompilerResult<Box<Stmt>> {
        let expr = self.construct_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(expr, last_token);
        Ok(stmt!(StmtType::Expr(expr), lexeme))
    }

    fn construct_dtype(&mut self) -> CompilerResult<DataType> {
        let dtype = self.consume_token(TokenType::Identifier)?;

        use std::ops::Deref;
        match dtype.lexeme().deref() {
            "int" => Ok(DataType::Int),
            "float" => Ok(DataType::Float),
            "bool" => Ok(DataType::Bool),
            _ => err!("Invalid type", dtype.lexeme()),
        }
    }

    /// BlockExpr := `{` Stmt* Expr? `}`
    fn construct_block_expr(&mut self) -> CompilerResult<Box<Expr>> {
        let lbrace = self.consume_token(TokenType::LBrace)?;

        let mut stmts = Vec::new();

        // construct statements and optional ending expression.
        let ending_expr = loop {
            // Loop until statements (except expression statements) can be constructed.
            if let Some(stmt) = self.try_construct_stmt() {
                stmts.push(stmt?);
                continue;
            }

            // A statement could not be constructed, so we check whether the block has ended.
            if let Some(tok) = self.peek_token()? {
                if let TokenType::RBrace = tok.token() {
                    break None; // No ending expression.
                }
            }

            // The block did not end, so we check for an expression (only possible syntactical construct here).
            let expr = self.construct_expr()?;

            // The above expression was either part of an expression statement, or the ending expression,
            // so we check for that. These are the only two alternatives.
            if let Some(tok) = self.peek_token()? {
                if let TokenType::RBrace = tok.token() {
                    break Some(expr); // Ending expression.
                }
            }

            // The block did not end, so it must have been an expression statement.
            // We consume the semicolon and continue parsing more statements.
            let semicolon = self.consume_token(TokenType::Semicolon)?;
            let lexeme = span_lexeme!(expr, semicolon);
            stmts.push(stmt!(StmtType::Expr(expr), lexeme));
        };

        let rbrace = self.consume_token(TokenType::RBrace)?;

        Ok(untyped_expr!(ExprType::Block(stmts, ending_expr), span_lexeme!(lbrace, rbrace)))
    }

    /// 
    /// Expr := Expr (`+`) Expr \
    /// Expr := (`+`) Expr \
    /// Expr := `(` Expr `)`
    /// 
    fn construct_expr(&mut self) -> CompilerResult<Box<Expr>> {
        self.construct_expr_recursive(OperatorPrecedence::None as u8)
    }
}

//=======================================
//          EXPR PARSING METHODS
//=======================================

impl<'a> Parser<'a> {
    fn construct_expr_recursive(&mut self, min_bp: u8) -> CompilerResult<Box<Expr>> {
        // Block Expressions handled specially.
        if let Some(tok) = self.peek_token()? {
            if let TokenType::LBrace = tok.token() {
                return self.construct_block_expr();
            }
        }

        // Assumes first token of expression has been validated.
        let tok = self.expect_token()?;
        let mut lhs = match tok.token() {
            TokenType::IntegerLiteral => untyped_expr!(ExprType::from_integer_literal(tok.lexeme())?, tok.lexeme().clone()),

            TokenType::FloatLiteral => untyped_expr!(ExprType::from_float_literal(tok.lexeme())?, tok.lexeme().clone()),

            TokenType::True => untyped_expr!(ExprType::BoolLiteral(true), tok.lexeme().clone()),
            TokenType::False => untyped_expr!(ExprType::BoolLiteral(false), tok.lexeme().clone()),

            TokenType::Identifier => untyped_expr!(ExprType::Identifier, tok.lexeme().clone()),

            TokenType::LParen => {
                let lhs = self.construct_expr_recursive(OperatorPrecedence::None as u8)?;
                let token_rparen = self.consume_token(TokenType::RParen)?;

                let lexeme = span_lexeme!(tok, token_rparen);
                untyped_expr!(lhs.take_expr(), lexeme)
            },

            t if t.as_prefix_operator().is_some() => {
                let op = unsafe { t.as_prefix_operator().unwrap_unchecked() };
                let ((), rbp) = op.prefix_binding_power();
                let rhs = self.construct_expr_recursive(rbp)?;

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
                    TokenType::Minus => ExprOperator::Subtract,
                    TokenType::Star => ExprOperator::Multiply,
                    TokenType::Slash => ExprOperator::Divide,
                    TokenType::Modulo => ExprOperator::Remainder,

                    TokenType::Lesser => ExprOperator::Lesser,
                    TokenType::LesserEqual => ExprOperator::LesserEqual,
                    TokenType::Greater => ExprOperator::Greater,
                    TokenType::GreaterEqual => ExprOperator::GreaterEqual,
                    TokenType::EqualEqual => ExprOperator::Equal,
                    TokenType::NotEqual => ExprOperator::NotEqual,

                    TokenType::And => ExprOperator::And,
                    TokenType::Or => ExprOperator::Or,

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
                    let rhs = self.construct_expr_recursive(0)?;
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

                let rhs = self.construct_expr_recursive(rbp)?;

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
            ExprOperator::UnaryPlus
            | ExprOperator::UnaryMinus 
            | ExprOperator::Not => ((), OperatorPrecedence::Unary.bp().1),
            op => unreachable!("prefix_binding_power(): {:?}", op),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            ExprOperator::Multiply
            | ExprOperator::Divide
            | ExprOperator::Remainder => Some(OperatorPrecedence::Multiply.bp()),

            ExprOperator::Add
            | ExprOperator::Subtract => Some(OperatorPrecedence::Add.bp()),

            ExprOperator::Lesser
            | ExprOperator::LesserEqual
            | ExprOperator::Greater
            | ExprOperator::GreaterEqual => Some(OperatorPrecedence::LesserGreater.bp()),

            ExprOperator::Equal
            | ExprOperator::NotEqual => Some(OperatorPrecedence::Equal.bp()),

            ExprOperator::And => Some(OperatorPrecedence::And.bp()),
            ExprOperator::Or => Some(OperatorPrecedence::Or.bp()),

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
            | TokenType::Identifier
            | TokenType::True
            | TokenType::False
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::LParen
            | TokenType::LBrace
            => true,
            _ => false,
        }
    }

    fn as_prefix_operator(&self) -> Option<ExprOperator> {
        match self {
            TokenType::Plus => Some(ExprOperator::UnaryPlus),
            TokenType::Minus => Some(ExprOperator::UnaryMinus),
            TokenType::Not => Some(ExprOperator::Not),
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
            ExprOperator::UnaryPlus | ExprOperator::UnaryMinus | ExprOperator::Not => {
                assert_eq!(operand_count, 1, "{}", msg);
                let expr = unsafe { operands.next().unwrap_unchecked() };
                match operator {
                    ExprOperator::UnaryPlus => ExprType::Positive(expr),
                    ExprOperator::UnaryMinus => ExprType::Negative(expr),
                    ExprOperator::Not => ExprType::Not(expr),
                    _ => unreachable!(),
                }
            },

            // Binary
            ExprOperator::Add | ExprOperator::Subtract | ExprOperator::Multiply | ExprOperator::Divide
            | ExprOperator::Remainder | ExprOperator::Lesser | ExprOperator::LesserEqual | ExprOperator::Greater
            | ExprOperator::GreaterEqual | ExprOperator::Equal | ExprOperator::NotEqual
            | ExprOperator::And | ExprOperator::Or | ExprOperator::Subscript => {
                assert_eq!(operand_count, 2, "{}", msg);
                let l = unsafe { operands.next().unwrap_unchecked() };
                let r = unsafe { operands.next().unwrap_unchecked() };
                match operator { 
                    ExprOperator::Add => ExprType::Add { l, r },
                    ExprOperator::Subtract => ExprType::Subtract { l, r },
                    ExprOperator::Multiply => ExprType::Multiply { l, r },
                    ExprOperator::Divide => ExprType::Divide { l, r },
                    ExprOperator::Remainder => ExprType::Remainder { l, r },

                    ExprOperator::Lesser => ExprType::Lesser { l, r },
                    ExprOperator::LesserEqual => ExprType::LesserEqual { l, r },
                    ExprOperator::Greater => ExprType::Greater { l, r },
                    ExprOperator::GreaterEqual => ExprType::GreaterEqual { l, r },
                    ExprOperator::Equal => ExprType::Equal { l, r },
                    ExprOperator::NotEqual => ExprType::NotEqual { l, r },

                    ExprOperator::And => ExprType::And { l, r },
                    ExprOperator::Or => ExprType::Or { l, r },

                    ExprOperator::Subscript => todo!(),
                    _ => unreachable!(),
                }
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

impl<'a> Parser<'a> {
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