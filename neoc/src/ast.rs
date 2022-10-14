use std::fmt::Debug;

//=========================================
// TYPES
//=========================================

pub struct Ast {
    ast: Vec<Box<Stmt>>,
}

pub enum Stmt {
    Expr(Box<Expr>),
    Nop,
}

pub enum Expr {
    BinaryOp(BinaryOpType, Box<Expr>, Box<Expr>),
    Int32(i32),
    Error, // Add custom errors
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
}


//=========================================
// IMPLEMENTATIONS
//=========================================

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for stmt in &self.ast {
            self.fmt_recurse_stmt(f, stmt, 0)?;
        }
        Ok(())
    }
}

impl Ast {
    pub fn new(ast: Vec<Box<Stmt>>) -> Self {
        Self { ast }
    }

    pub fn unwrap(self) -> Vec<Box<Stmt>> { self.ast }

    fn fmt_recurse_stmt(&self, f: &mut std::fmt::Formatter, stmt: &Box<Stmt>, indent: usize) -> Result<(), std::fmt::Error> {
        use Stmt::*;
        let next_indent = indent + 2;
        match stmt.as_ref() {
            Expr(expr) => {
                writeln!(f, "{:indent$}Expr", "", indent=indent)?;
                self.fmt_recurse_expr(f, expr, next_indent)
            },
            Nop => writeln!(f, "{:indent$}Nop", "", indent=indent)
        }
    }

    fn fmt_recurse_expr(&self, f: &mut std::fmt::Formatter, expr: &Box<Expr>, indent: usize) -> Result<(), std::fmt::Error> {
        use Expr::*;
        let next_indent = indent + 2;
        match expr.as_ref() {
            BinaryOp(op, l, r) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent=indent)?;
                self.fmt_recurse_expr(f, l, next_indent)?;
                self.fmt_recurse_expr(f, r, next_indent)
            },
            Int32(i) => writeln!(f, "{:indent$}i32( {:?} )", "", i, indent=indent),
            Error => writeln!(f, "{:indent$}ERROR", "", indent=indent),
        }
    }
}