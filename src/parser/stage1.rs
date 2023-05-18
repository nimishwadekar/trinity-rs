use crate::{lexer::{TokenStream, Token, TokenType, Lexeme}, CompilerResult};

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
        Box::new(StmtV1::new($s, $lexeme))
    };
}

macro_rules! expr {
    ($e:expr, $lexeme:expr) => {
        Box::new(ExprV1::new($e, $lexeme))
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

#[derive(Debug)]
pub enum ExprTypeV1 {
    IntegerLiteral(Lexeme),

    // Unary Operations.
    Positive(Box<ExprV1>),

    // Binary Operations.
    Add{ l: Box<ExprV1>, r: Box<ExprV1> },

    // Ternary Operations.
}

#[derive(Debug)]
pub struct ExprV1 {
    expr: ExprTypeV1,
    lexeme: Lexeme,
}

#[derive(Debug)]
pub enum StmtTypeV1 {
    Expr(Box<ExprV1>),
    Print(Box<ExprV1>),
}

#[derive(Debug)]
pub struct StmtV1 {
    stmt: StmtTypeV1,
    lexeme: Lexeme,
}

pub struct ParseTreeV1 {
    stmts: Vec<Box<StmtV1>>,
}

pub struct ParserV1<'a> {
    tokens: TokenStream<'a>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for ParseTreeV1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.stmts.iter() {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for StmtV1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

impl std::fmt::Display for ExprV1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> ParserV1<'a> {
    pub fn parse(tokens: TokenStream<'a>)-> CompilerResult<ParseTreeV1> {
        let mut parser = Self { tokens };
        Ok(ParseTreeV1 { stmts: parser.parse_program()? })
    }

    fn parse_program(&mut self) -> Result<Vec<Box<StmtV1>>, String> {
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

                    _ => return Err(err_format("Invalid start of statement", token)),
                }
            };
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    /// PrintStmt := `print` Expr `;`
    fn parse_print_stmt(&mut self) -> CompilerResult<Box<StmtV1>> {
        let first_token = self.consume_token(TokenType::Print)?;
        let expr = self.parse_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(first_token, last_token);
        Ok(stmt!(StmtTypeV1::Print(expr), lexeme))
    }

    /// ExprStmt := Expr `;`
    fn parse_expr_stmt(&mut self) -> CompilerResult<Box<StmtV1>> {
        let expr = self.parse_expr()?;
        let last_token = self.consume_token(TokenType::Semicolon)?;

        let lexeme = span_lexeme!(expr, last_token);
        Ok(stmt!(StmtTypeV1::Expr(expr), lexeme))
    }

    /// Expr := Expr (`+`) Expr \
    /// Expr := (`+`) Expr \
    /// Expr := `(` Expr `)`
    fn parse_expr(&mut self) -> CompilerResult<Box<ExprV1>> {
        self.parse_expr_recursive(OperatorPrecedence::None as u8)
    }
}

impl ParseTreeV1 {
    pub fn stmts(&self) -> &Vec<Box<StmtV1>> {
        &self.stmts
    }
}

impl StmtV1 {
    fn new(stmt: StmtTypeV1, lexeme: Lexeme) -> Self {
        Self { stmt, lexeme }
    }

    pub fn stmt(&self) -> &StmtTypeV1 {
        &self.stmt
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}

impl ExprV1 {
    fn new(expr: ExprTypeV1, lexeme: Lexeme) -> Self {
        Self { expr, lexeme }
    }

    pub fn expr(&self) -> &ExprTypeV1 {
        &self.expr
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}

//=======================================
//          EXPR PARSING METHODS
//=======================================

impl<'a> ParserV1<'a> {
    fn parse_expr_recursive(&mut self, min_bp: u8) -> CompilerResult<Box<ExprV1>> {
        // Assumes first token of expression has been validated.
        let tok = self.expect_token()?;
        let mut lhs = match tok.token() {
            TokenType::IntegerLiteral(value) => Box::new(ExprV1::new(ExprTypeV1::IntegerLiteral(value.clone()), tok.lexeme().clone())),
            TokenType::LParen => {
                let lhs = self.parse_expr_recursive(OperatorPrecedence::None as u8)?;
                let token_rparen = self.consume_token(TokenType::RParen)?;

                let lexeme = span_lexeme!(tok, token_rparen);
                expr!(lhs.expr, lexeme)
            },
            t if t.as_prefix_operator().is_some() => {
                let op = unsafe { t.as_prefix_operator().unwrap_unchecked() };
                let ((), rbp) = op.prefix_binding_power();
                let rhs = self.parse_expr_recursive(rbp)?;

                let lexeme = span_lexeme!(tok, rhs);
                expr!(ExprTypeV1::new(op, vec![rhs]), lexeme)
            },

            _ => return Err(err_format("Expected expression", tok)),
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
                    expr!(ExprTypeV1::new(op, vec![lhs, rhs]), lexeme)
                } else {
                    let lexeme = span_lexeme!(lhs, token_op);
                    expr!(ExprTypeV1::new(op, vec![lhs]), lexeme)
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
                lhs = expr!(ExprTypeV1::new(op, vec![lhs, rhs]), lexeme);
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

impl ExprTypeV1 {
    fn new(operator: ExprOperator, operands: Vec<Box<ExprV1>>) -> Self {
        let operand_count = operands.len();
        let mut operands = operands.into_iter();
        let msg = format!("`{operator:?}` with {operand_count} operand{}", if operand_count == 1 {""} else {"s"});

        match operator {
            // Unary
            ExprOperator::UnaryPlus => {
                assert_eq!(operand_count, 1, "{}", msg);
                let expr = unsafe { operands.next().unwrap_unchecked() };
                ExprTypeV1::Positive(expr)
            },

            // Binary
            ExprOperator::Add => {
                assert_eq!(operand_count, 2, "{}", msg);
                let l = unsafe { operands.next().unwrap_unchecked() };
                let r = unsafe { operands.next().unwrap_unchecked() };
                ExprTypeV1::Add{ l, r }
            },
            ExprOperator::Subscript => {
                assert_eq!(operand_count, 2, "{}", msg);
                todo!("subscript expression")
            },
        }
    }
}

//=======================================
//          HELPER METHODS
//=======================================

impl<'a> ParserV1<'a> {
    #[inline]
    fn peek_token(&mut self) -> CompilerResult<Option<Token>> {
        match self.tokens.peek() {
            None => Ok(None),
            Some(t) => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
                _ => Ok(Some(t)),
            },
        }
    }

    #[inline]
    fn next_token(&mut self) -> CompilerResult<Option<Token>> {
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
    fn expect_token(&mut self) -> CompilerResult<Token> {
        let t = self.tokens.next();
        match self.handle_eof_token(t)? {
            t => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
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
            TokenType::Error(..) => Err(err_format("Unknown character", token)),
            t if t == &expected => Ok(token),
            t => Err(format!("<{}> Expected `{}`, found `{}`", token.lexeme().location(), expected, t)),
        }
    }

    #[inline(always)]
    fn handle_eof_token(&self, token: Option<Token>) -> CompilerResult<Token> {
        token.ok_or(format!("<{}> Unexpected end of file", self.tokens.location()))
    }
}

//=======================================
//          FORMAT METHODS
//=======================================

const NEXT_INDENT: usize = 3;

impl StmtV1 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self.stmt() {
            StmtTypeV1::Expr(expr) => {
                writeln!(f, "ExprStmt")?;
                expr.display_format(f, indent)?;
            },
            StmtTypeV1::Print(expr) => {
                writeln!(f, "PrintStmt")?;
                expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

impl ExprV1 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self.expr() {
            ExprTypeV1::IntegerLiteral(value) => writeln!(f, "Integer {}", value)?,
            ExprTypeV1::Positive(expr) => {
                writeln!(f, "Positive")?;
                expr.display_format(f, indent)?;
            },
            ExprTypeV1::Add { l, r } => {
                writeln!(f, "Add")?;
                l.display_format(f, indent)?;
                r.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

#[inline(always)]
fn err_format(err: &str, t: Token) -> String {
    format!("<{}> `{}`: {}", t.lexeme().location(), t.lexeme(), err)
}