use crate::CompilerResult;
use crate::lexer::{Token, Lexeme};
use crate::parser::stage1::{ParseTreeV1, StmtV1, StmtTypeV1, ExprV1, ExprTypeV1};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    I32,
}

#[derive(Debug)]
pub enum ExprTypeV2 {
    IntegerLiteral(Lexeme),

    // Unary Operations.
    Positive(Box<ExprV2>),

    // Binary Operations.
    Add{ l: Box<ExprV2>, r: Box<ExprV2> },

    // Ternary Operations.
}

#[derive(Debug)]
pub enum StmtTypeV2 {
    Expr(Box<ExprV2>),
    Print(Box<ExprV2>),
}

#[derive(Debug)]
pub struct ExprV2 {
    expr: ExprTypeV2,
    data_type: DataType,
    lexeme: Lexeme,
}

#[derive(Debug)]
pub struct StmtV2 {
    stmt: StmtTypeV2,
    lexeme: Lexeme,
}

pub struct ParseTreeV2 {
    stmts: Vec<Box<StmtV2>>,
}

pub struct ParserV2;

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for ParseTreeV2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.stmts.iter() {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for StmtV2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stmt.display_format(f, 0)
    }
}

impl std::fmt::Display for ExprV2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expr.display_format(f, 0)
    }
}

impl std::fmt::Display for StmtTypeV2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

impl std::fmt::Display for ExprTypeV2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_format(f, 0)
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl ParserV2 {
    pub fn parse(tree: ParseTreeV1)-> CompilerResult<ParseTreeV2> {
        let mut parser = Self;
        let mut stmts = Vec::new();
        for stmt in tree.stmts() {
            stmts.push(parser.parse_stmt(stmt)?);
        }
        Ok(ParseTreeV2 { stmts })
    }

    fn parse_stmt(&mut self, stmt: &StmtV1) -> CompilerResult<Box<StmtV2>> {
        let stmt_type = match stmt.stmt() {
            StmtTypeV1::Expr(expr) => StmtTypeV2::Expr(self.parse_expr(expr)?),
            StmtTypeV1::Print(expr) => StmtTypeV2::Print(self.parse_expr(expr)?),
        };
        Ok(Box::new(StmtV2::new(stmt_type, stmt.lexeme().clone())))
    }

    fn parse_expr(&mut self, expr: &ExprV1) -> CompilerResult<Box<ExprV2>> {
        let (expr_type, data_type) = match expr.expr() {
            ExprTypeV1::IntegerLiteral(lexeme) => (ExprTypeV2::IntegerLiteral(lexeme.clone()), self.get_integer_type(lexeme)?),
            
            ExprTypeV1::Add { l, r } => {
                let l = self.parse_expr(l)?;
                let r = self.parse_expr(r)?;
                
                // Do type checking for addition.

                let data_type = l.data_type;
                (ExprTypeV2::Add { l, r }, data_type)
            },

            ExprTypeV1::Positive(expr) => {
                let expr = self.parse_expr(expr)?;
                // Type check

                let data_type = expr.data_type;
                (ExprTypeV2::Positive(expr), data_type)
            }
        };

        Ok(Box::new(ExprV2::new(expr_type, data_type, expr.lexeme().clone())))
    }

    fn get_integer_type(&self, lexeme: &Lexeme) -> CompilerResult<DataType> {
        Ok(DataType::I32)
    }
}

impl ParseTreeV2 {
    pub fn stmts(&self) -> &Vec<Box<StmtV2>> {
        &self.stmts
    }
}

impl StmtV2 {
    fn new(stmt: StmtTypeV2, lexeme: Lexeme) -> Self {
        Self { stmt, lexeme }
    }

    pub fn stmt(&self) -> &StmtTypeV2 {
        &self.stmt
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}

impl ExprV2 {
    fn new(expr: ExprTypeV2, data_type: DataType, lexeme: Lexeme) -> Self {
        Self { expr, data_type, lexeme }
    }

    pub fn expr(&self) -> &ExprTypeV2 {
        &self.expr
    }

    pub fn data_type(&self) -> &DataType {
        &self.data_type
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}

//=======================================
//          FORMAT METHODS
//=======================================

const NEXT_INDENT: usize = 3;

impl StmtTypeV2 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self {
            StmtTypeV2::Expr(expr) => {
                writeln!(f, "ExprStmt")?;
                expr.expr.display_format(f, indent)?;
            },
            StmtTypeV2::Print(expr) => {
                writeln!(f, "PrintStmt")?;
                expr.expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

impl ExprTypeV2 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self {
            ExprTypeV2::IntegerLiteral(value) => writeln!(f, "Integer {}", value)?,
            ExprTypeV2::Positive(expr) => {
                writeln!(f, "Positive")?;
                expr.expr.display_format(f, indent)?;
            },
            ExprTypeV2::Add { l, r } => {
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