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
    Bool,
}

#[derive(Debug)]
pub enum ExprType {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),

    // Unary Operations.
    Positive(Box<Expr>),
    Negative(Box<Expr>),
    Not(Box<Expr>),

    // Binary Operations.
    Add { l: Box<Expr>, r: Box<Expr> },
    Subtract { l: Box<Expr>, r: Box<Expr> },
    Multiply { l: Box<Expr>, r: Box<Expr> },
    Divide { l: Box<Expr>, r: Box<Expr> },
    Remainder { l: Box<Expr>, r: Box<Expr> },

    Lesser { l: Box<Expr>, r: Box<Expr> },
    LesserEqual { l: Box<Expr>, r: Box<Expr> },
    Greater { l: Box<Expr>, r: Box<Expr> },
    GreaterEqual { l: Box<Expr>, r: Box<Expr> },
    Equal { l: Box<Expr>, r: Box<Expr> },
    NotEqual { l: Box<Expr>, r: Box<Expr> },

    And { l: Box<Expr>, r: Box<Expr> },
    Or { l: Box<Expr>, r: Box<Expr> },

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

impl DataType {
    #[inline(always)]
    pub fn is_int(&self) -> bool {
        *self == DataType::Int
    }

    #[inline(always)]
    pub fn is_float(&self) -> bool {
        *self == DataType::Float
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        *self == DataType::Bool
    }

    #[inline(always)]
    pub fn is_numeric_primitive(&self) -> bool {
        self.is_int() || self.is_float()
    }

    #[inline(always)]
    pub fn is_primitive(&self) -> bool {
        self.is_numeric_primitive() || self.is_bool()
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
            ExprType::BoolLiteral(value) => writeln!(f, "Bool {}", value)?,

            ExprType::Positive(expr)
            | ExprType::Negative(expr)
            | ExprType::Not(expr) => {
                writeln!(f, "{}", match self.expr() {
                    ExprType::Positive(..) => "Positive",
                    ExprType::Negative(..) => "Negative",
                    ExprType::Not(..) => "Not",
                    _ => unreachable!(),
                })?;
                expr.display_format(f, indent)?;
            },

            ExprType::Add { l, r }
            | ExprType::Subtract { l, r }
            | ExprType::Multiply { l, r }
            | ExprType::Divide { l, r }
            | ExprType::Remainder { l, r }
            
            | ExprType::Lesser { l, r }
            | ExprType::LesserEqual { l, r }
            | ExprType::Greater { l, r }
            | ExprType::GreaterEqual { l, r }
            | ExprType::Equal { l, r }
            | ExprType::NotEqual { l, r }
            
            | ExprType::And { l, r }
            | ExprType::Or { l, r } => {
                writeln!(f, "{}", match self.expr() {
                    ExprType::Add {..} => "Add",
                    ExprType::Subtract {..} => "Subtract",
                    ExprType::Multiply {..} => "Multiply",
                    ExprType::Divide {..} => "Divide",
                    ExprType::Remainder {..} => "Remainder",
                    
                    ExprType::Lesser {..} => "Lesser",
                    ExprType::LesserEqual {..} => "LesserEqual",
                    ExprType::Greater {..} => "Greater",
                    ExprType::GreaterEqual {..} => "GreaterEqual",
                    ExprType::Equal {..} => "Equal",
                    ExprType::NotEqual {..} => "NotEqual",
                    
                    ExprType::And {..} => "And",
                    ExprType::Or {..} => "Or",
                    _ => unreachable!(),
                })?;
                l.display_format(f, indent)?;
                r.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}