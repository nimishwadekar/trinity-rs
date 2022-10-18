use std::fmt::Debug;

//=========================================
// TYPES
//=========================================

pub struct ParseTree {
    tree: Vec<Box<Stmt>>,
}

pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Nop,
}

pub enum Expr {
    ArithmeticBinOp(ArithmeticBinOpType, Box<Expr>, Box<Expr>),
    Int(i64),
    Bool(bool),
    Float(f64),
    Nil,
    Error,
}

#[derive(Copy, Clone, Debug)]
pub enum ArithmeticBinOpType {
    Add,
    Sub,
    Mul,
    Div,
}

//=========================================
// TRAIT IMPLEMENTATIONS
//=========================================

impl Debug for ParseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "ParseTree:")?;
        for stmt in &self.tree {
            self.fmt_recurse_stmt(f, stmt, 0)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ArithmeticBinOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ArithmeticBinOpType::Add => write!(f, "+"),
            ArithmeticBinOpType::Sub => write!(f, "-"),
            ArithmeticBinOpType::Mul => write!(f, "*"),
            ArithmeticBinOpType::Div => write!(f, "/"),
        }
    }
}

//=========================================
// IMPLEMENTATIONS
//=========================================

impl ParseTree {
    pub fn new(tree: Vec<Box<Stmt>>) -> Self {
        Self { tree }
    }

    pub fn unwrap(self) -> Vec<Box<Stmt>> { self.tree }

    fn fmt_recurse_stmt(&self, f: &mut std::fmt::Formatter, stmt: &Box<Stmt>, indent: usize) -> Result<(), std::fmt::Error> {
        use Stmt::*;
        let next_indent = indent + 2;
        match stmt.as_ref() {
            Expr(expr) => {
                writeln!(f, "{:indent$}Expr", "", indent=indent)?;
                self.fmt_recurse_expr(f, expr, next_indent)
            },

            Print(expr) => {
                writeln!(f, "{:indent$}Print", "", indent=indent)?;
                self.fmt_recurse_expr(f, expr, next_indent)
            },

            Nop => writeln!(f, "{:indent$}Nop", "", indent=indent)
        }
    }

    fn fmt_recurse_expr(&self, f: &mut std::fmt::Formatter, expr: &Box<Expr>, indent: usize) -> Result<(), std::fmt::Error> {
        use Expr::*;
        let next_indent = indent + 2;
        match expr.as_ref() {
            ArithmeticBinOp(op, l, r) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent=indent)?;
                self.fmt_recurse_expr(f, l, next_indent)?;
                self.fmt_recurse_expr(f, r, next_indent)
            },

            Int(val) => writeln!(f, "{:indent$}int( {:?} )", "", val, indent=indent),

            Bool(val) => writeln!(f, "{:indent$}bool( {:?} )", "", val, indent=indent),

            Float(val) => writeln!(f, "{:indent$}float( {:?} )", "", val, indent=indent),

            Nil => writeln!(f, "{:indent$}nil", "", indent=indent),

            Error => writeln!(f, "{:indent$}error", "", indent=indent),
        }
    }
}