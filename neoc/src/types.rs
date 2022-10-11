use std::fmt::Debug;

//=========================================
// TYPES
//=========================================

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
