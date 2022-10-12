use std::fmt::Debug;

//=========================================
// TYPES
//=========================================

pub struct Ast {
    ast: Box<Expr>,
}

#[derive(Debug)]
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

impl Ast {
    pub fn new(ast: Box<Expr>) -> Self {
        Self { ast }
    }

    pub fn pretty_print_stdout(&self) {
        self.recursive_pretty_print_stdout(&self.ast, 0);
    }

    fn recursive_pretty_print_stdout(&self, expr: &Box<Expr>, indent: usize) {
        use Expr::*;
        match expr.as_ref() {
            BinaryOp(op, l, r) => {
                println!("{:indent$}{:?}", "", op, indent=indent);
                self.recursive_pretty_print_stdout(l, indent + 2);
                self.recursive_pretty_print_stdout(r, indent + 2);
            },
            Int32(i) => println!("{:indent$}Int32( {:?} )", "", i, indent=indent),
            Error => println!("{:indent$}error", "", indent=indent),
        }
    }
}