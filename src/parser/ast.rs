use std::cell::Cell;

use crate::lexer::Lexeme;

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
    /// Equivalent to no type.
    Unit,
    Int,
    Float,
}

#[derive(Debug)]
pub enum ExprType {
    IntegerLiteral(i64),
    FloatLiteral(f64),

    // Unary Operations.
    Positive(Box<Expr>),

    // Binary Operations.
    Add{ l: Box<Expr>, r: Box<Expr> },

    // Ternary Operations.
}

#[derive(Debug)]
pub enum StmtType {
    Expr(Box<Expr>),
    Print(Box<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    expr: ExprType,
    dtype: Cell<DataType>,
    lexeme: Lexeme,
}

#[derive(Debug)]
pub struct Stmt {
    stmt: StmtType,
    lexeme: Lexeme,
}

pub struct ParseTree {
    stmts: Vec<Box<Stmt>>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for ParseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.stmts.iter() {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
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

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl ParseTree {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &Vec<Box<Stmt>> {
        &self.stmts
    }
}

impl Stmt {
    pub fn new(stmt: StmtType, lexeme: Lexeme) -> Self {
        Self { stmt, lexeme }
    }

    pub fn stmt(&self) -> &StmtType {
        &self.stmt
    }
}

impl Expr {
    pub fn new(expr: ExprType, dtype: DataType, lexeme: Lexeme) -> Self {
        Self { expr, dtype: Cell::new(dtype), lexeme }
    }

    pub fn expr(&self) -> &ExprType {
        &self.expr
    }

    pub fn take_expr(self) -> ExprType {
        self.expr
    }

    pub fn dtype(&self) -> DataType {
        self.dtype.get()
    }

    pub fn set_dtype(&self, dtype: DataType) {
        self.dtype.set(dtype);
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
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
        match self.stmt() {
            StmtType::Expr(expr) => {
                writeln!(f, "ExprStmt")?;
                expr.display_format(f, indent)?;
            },
            StmtType::Print(expr) => {
                writeln!(f, "PrintStmt")?;
                expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

impl Expr {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$} <{}> ", "", self.dtype.get())?;
        let indent = indent + NEXT_INDENT;
        match self.expr() {
            ExprType::IntegerLiteral(value) => writeln!(f, "Integer {}", value)?,
            ExprType::FloatLiteral(value) => writeln!(f, "Float {}", value)?,
            ExprType::Positive(expr) => {
                writeln!(f, "Positive")?;
                expr.display_format(f, indent)?;
            },
            ExprType::Add { l, r } => {
                writeln!(f, "Add")?;
                l.display_format(f, indent)?;
                r.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}