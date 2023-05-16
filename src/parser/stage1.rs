use crate::lexer::{TokenStream, Token, TokenType, Lexeme};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================

macro_rules! stmt_node {
    ($s:expr, $lexeme:expr) => {
        Box::new(StmtNode::new($s, $lexeme))
    };
}

macro_rules! expr_node {
    ($e:expr, $lexeme:expr) => {
        Box::new(ExprNode::new($e, $lexeme))
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
pub enum Expr {
    IntegerLiteral(i32),

    // Unary Operations.
    Positive(Box<ExprNode>),

    // Binary Operations.
    Add{ l: Box<ExprNode>, r: Box<ExprNode> },

    // Ternary Operations.
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Box<ExprNode>),
    Print(Box<ExprNode>),
}

#[derive(Debug)]
pub struct ExprNode {
    pub expr: Expr,
    lexeme: Lexeme,
}

#[derive(Debug)]
pub struct StmtNode {
    pub stmt: Stmt,
    lexeme: Lexeme,
}

pub struct Stage1Tree {
    stmts: Vec<Box<StmtNode>>,
}

pub struct Stage1Parser<'a> {
    tokens: TokenStream<'a>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for Stage1Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.stmts.iter() {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for StmtNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stmt.display_format(f, 0)
    }
}

impl std::fmt::Display for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expr.display_format(f, 0)
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl<'a> Stage1Parser<'a> {
    pub fn new(tokens: TokenStream<'a>) -> Self {
        Self {
            tokens,
        }
    }

    pub fn parse(&mut self)-> Result<Stage1Tree, String> {
        Ok(Stage1Tree { stmts: self.parse_program()? })
    }

    fn parse_program(&mut self) -> Result<Vec<Box<StmtNode>>, String> {
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
    fn parse_print_stmt(&mut self) -> Result<Box<StmtNode>, String> {
        let token_print = self.consume_token(TokenType::Print)?;
        let expr = self.parse_expr()?;
        let token_semicolon = self.consume_token(TokenType::Semicolon)?;

        /* Ok(Box::new(StmtNode::new(
            Stmt::Print(expr),
            token_print.lexeme().clone() + expr.lexeme + token_semicolon.lexeme().clone()
        ))) */
        let expr_lexeme = expr.lexeme.clone();
        Ok(stmt_node!(
            Stmt::Print(expr),
            token_print.lexeme().clone() + expr_lexeme + token_semicolon.lexeme().clone()
        ))
    }

    /// ExprStmt := Expr `;`
    fn parse_expr_stmt(&mut self) -> Result<Box<StmtNode>, String> {
        let expr = self.parse_expr()?;
        let token_semicolon = self.consume_token(TokenType::Semicolon)?;

        let expr_lexeme = expr.lexeme.clone();
        Ok(stmt_node!(
            Stmt::Expr(expr),
            expr_lexeme + token_semicolon.lexeme().clone()
        ))
    }

    /// Expr := Expr (`+`) Expr \
    /// Expr := (`+`) Expr \
    /// Expr := `(` Expr `)`
    fn parse_expr(&mut self) -> Result<Box<ExprNode>, String> {
        self.parse_expr_recursive(OperatorPrecedence::None as u8)
    }
}

impl Stage1Tree {
    pub fn stmts(&self) -> &Vec<Box<StmtNode>> {
        &self.stmts
    }
}

impl StmtNode {
    fn new(stmt: Stmt, lexeme: Lexeme) -> Self {
        Self { stmt, lexeme }
    }
}

impl ExprNode {
    fn new(expr: Expr, lexeme: Lexeme) -> Self {
        Self { expr, lexeme }
    }
}

//=======================================
//          EXPR PARSING METHODS
//=======================================

impl<'a> Stage1Parser<'a> {
    fn parse_expr_recursive(&mut self, min_bp: u8) -> Result<Box<ExprNode>, String> {
        // Assumes first token of expression has been validated.
        let tok = self.expect_token()?;
        let mut lhs = match tok.token() {
            TokenType::IntegerLiteral(value) => Box::new(ExprNode::new(Expr::IntegerLiteral(*value), tok.lexeme().clone())),
            TokenType::LParen => {
                let lhs = self.parse_expr_recursive(OperatorPrecedence::None as u8)?;
                let token_rparen = self.consume_token(TokenType::RParen)?;

                expr_node!(
                    lhs.expr,
                    tok.lexeme().clone() + lhs.lexeme + token_rparen.lexeme().clone()
                )
            },
            t if t.as_prefix_operator().is_some() => {
                let op = unsafe { t.as_prefix_operator().unwrap_unchecked() };
                let ((), rbp) = op.prefix_binding_power();
                let rhs = self.parse_expr_recursive(rbp)?;

                let rhs_lexeme = rhs.lexeme.clone();
                expr_node!(
                    Expr::new(op, vec![rhs]),
                    tok.lexeme().clone() + rhs_lexeme
                )
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

                    let (lhs_lexeme, rhs_lexeme) = (lhs.lexeme.clone(), rhs.lexeme.clone());
                    expr_node!(
                        Expr::new(op, vec![lhs, rhs]),
                        lhs_lexeme + token_op.lexeme().clone() + rhs_lexeme + token_rsquare.lexeme().clone()
                    )
                } else {

                    let lhs_lexeme = lhs.lexeme.clone();
                    expr_node!(
                        Expr::new(op, vec![lhs]),
                        lhs_lexeme + token_op.lexeme().clone()
                    )
                };
                continue;
            }

            if let Some((lbp, rbp)) = op.infix_binding_power() {
                if lbp < min_bp {
                    break;
                }
                self.next_token().unwrap();

                let rhs = self.parse_expr_recursive(rbp)?;

                let (lhs_lexeme, rhs_lexeme) = (lhs.lexeme.clone(), rhs.lexeme.clone());
                lhs = expr_node!(
                    Expr::new(op, vec![lhs, rhs]),
                    lhs_lexeme + token_op.lexeme().clone() + rhs_lexeme
                );
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

impl Expr {
    fn new(operator: ExprOperator, operands: Vec<Box<ExprNode>>) -> Self {
        let operand_count = operands.len();
        let mut operands = operands.into_iter();
        let msg = format!("`{operator:?}` with {operand_count} operand{}", if operand_count == 1 {""} else {"s"});

        match operator {
            // Unary
            ExprOperator::UnaryPlus => {
                assert_eq!(operand_count, 1, "{}", msg);
                let expr = unsafe { operands.next().unwrap_unchecked() };
                Expr::Positive(expr)
            },

            // Binary
            ExprOperator::Add => {
                assert_eq!(operand_count, 2, "{}", msg);
                let l = unsafe { operands.next().unwrap_unchecked() };
                let r = unsafe { operands.next().unwrap_unchecked() };
                Expr::Add{ l, r }
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

impl<'a> Stage1Parser<'a> {
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
        let t = self.tokens.next();
        match self.handle_eof_token(t)? {
            t => match t.token() {
                TokenType::Error(..) => Err(err_format("Unknown character", t)),
                _ => Ok(t),
            },
        }
    }

    #[inline]
    fn consume_token(&mut self, expected: TokenType) -> Result<Token, String> {
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
    fn handle_eof_token(&self, token: Option<Token>) -> Result<Token, String> {
        token.ok_or(format!("<{}> Unexpected end of file", self.tokens.location()))
    }
}

//=======================================
//          FORMAT METHODS
//=======================================

const NEXT_INDENT: usize = 3;

impl Stmt {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self {
            Stmt::Expr(expr) => {
                writeln!(f, "ExprStmt")?;
                expr.expr.display_format(f, indent)?;
            },
            Stmt::Print(expr) => {
                writeln!(f, "PrintStmt")?;
                expr.expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

impl Expr {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self {
            Expr::IntegerLiteral(value) => writeln!(f, "Integer {}", value)?,
            Expr::Positive(expr) => {
                writeln!(f, "Positive")?;
                expr.expr.display_format(f, indent)?;
            },
            Expr::Add { l, r } => {
                writeln!(f, "Add")?;
                l.expr.display_format(f, indent)?;
                r.expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

#[inline(always)]
fn err_format(err: &str, t: Token) -> String {
    format!("<{}> `{}`: {}", t.lexeme().location(), t.lexeme(), err)
}