use std::fmt::Debug;
use crate::{parsetree as parse, error::CompileError, types::DataType};
use parse::ParseTree;
use super::symbols::SymbolTable;
pub use parse::ArithmeticBinOpType;

//=========================================
// TYPES
//=========================================

pub struct Ast {
    stmts: Vec<Box<Stmt>>,

    symbols: SymbolTable,
}

pub enum Stmt {
    Expr(Box<TypedExpr>),
    Print(Box<TypedExpr>),
    GlobalDecl{ ident: String, dtype: DataType, val: Box<TypedExpr> },
    Nop,
}

pub enum Expr {
    ArithmeticBinOp(ArithmeticBinOpType, Box<TypedExpr>, Box<TypedExpr>),
    Variable(String),
    Literal(Literal),
}

pub struct TypedExpr {
    /// Data type.
    pub dtype: DataType,
    pub expr: Expr,
}

pub enum Literal {
    Int(i64),
    Bool(bool),
    Float(f64),
    Nil,
}

//=========================================
// TRAIT IMPLEMENTATIONS
//=========================================

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Literal::Int(val) => write!(f, "{:?}", val),
            Literal::Bool(val) => write!(f, "{:?}", val),
            Literal::Float(val) => write!(f, "{:?}", val),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "AST:")?;
        for stmt in &self.stmts {
            self.fmt_recurse_stmt(f, stmt, 2)?;
        }
        Ok(())
    }
}

// Helper methods for Debug.
impl Ast {
    fn fmt_recurse_stmt(&self, f: &mut std::fmt::Formatter, stmt: &Box<Stmt>, indent: usize) -> Result<(), std::fmt::Error> {
        use Stmt::*;
        let next_indent = indent + 2;
        match stmt.as_ref() {
            Expr(expr) => {
                writeln!(f, "{:indent$}Expr", "", indent=indent)?;
                self.fmt_recurse_typed_expr(f, expr, next_indent)
            },

            Print(expr) => {
                writeln!(f, "{:indent$}Print", "", indent=indent)?;
                self.fmt_recurse_typed_expr(f, expr, next_indent)
            },

            GlobalDecl { ident, dtype, val } => {
                writeln!(f, "{:indent$}GlobalDecl \"{ident}\" <{:?}>", "", dtype, indent=indent)?;
                self.fmt_recurse_typed_expr(f, val, next_indent)
            }

            Nop => writeln!(f, "{:indent$}Nop", "", indent=indent)
        }
    }

    fn fmt_recurse_typed_expr(&self, f: &mut std::fmt::Formatter, expr: &Box<TypedExpr>, indent: usize) -> Result<(), std::fmt::Error> {
        let TypedExpr{ dtype, expr } = expr.as_ref();
        write!(f, "{:indent$}{}  ", "", dtype.to_string(), indent=indent)?;
        self.fmt_recurse_expr(f, expr, indent)
    }

    fn fmt_recurse_expr(&self, f: &mut std::fmt::Formatter, expr: &Expr, indent: usize) -> Result<(), std::fmt::Error> {
        use Expr::*;
        let next_indent = indent + 2;
        match expr {
            ArithmeticBinOp(op, l, r) => {
                writeln!(f, "{:?}", op)?;
                self.fmt_recurse_typed_expr(f, l, next_indent)?;
                self.fmt_recurse_typed_expr(f, r, next_indent)
            },

            Variable(ident) => writeln!(f, "var \"{ident}\""),

            Literal(val) => writeln!(f, "( {:?} )", val),
        }
    }
}

//=========================================
// IMPLEMENTATIONS
//=========================================

impl TypedExpr {
    fn new(dtype: DataType, expr: Expr) -> Self {
        Self { dtype, expr }
    }
}

#[allow(non_snake_case)]
impl Ast {
    //=========================================
    // STMT
    //=========================================

    fn from_stmt(&mut self, stmt: &Box<parse::Stmt>) -> Result<Box<Stmt>, CompileError> {
        Ok(Box::new(match stmt.as_ref() {
            parse::Stmt::Expr(parse_expr) => Stmt::Expr(self.from_expr(parse_expr)?),

            parse::Stmt::Print(parse_expr) => Stmt::Print(self.from_expr(parse_expr)?),

            parse::Stmt::GlobalDecl { ident, dtype, val } =>
                self.from_stmt_GlobalDecl(ident, dtype, val)?,

            parse::Stmt::Nop => Stmt::Nop,

            parse::Stmt::Error => panic!("Parse Tree with errors should not have made it to conversion to AST."),
        }))
    }

    fn from_stmt_GlobalDecl(&mut self, ident: &String, dtype: &String, val: &Box<parse::Expr>) -> Result<Stmt, CompileError> {
        let dtype = match DataType::try_primitive_from(dtype) {
            Some(dtype) => dtype,
            None => todo!("Non-primitive types"),
        };

        let val = self.from_expr(val)?;
        if dtype != val.dtype {
            return Err(CompileError::TypeMismatch(
                format!("Cannot assign value of type `{:?}` to variable of type `{:?}`", val.dtype, dtype)
            ))
        }

        if let Some(..) = self.symbols.add_variable(ident.to_string(), dtype) {
            return Err(CompileError::VariableRedeclared(
                format!("Variable `{}` has been declared twice", ident)
            ))
        }
        Ok(Stmt::GlobalDecl { ident: ident.to_string(), dtype, val })
    }

    //=========================================
    // EXPR
    //=========================================

    fn from_expr(&mut self, expr: &Box<parse::Expr>) -> Result<Box<TypedExpr>, CompileError> {
        Ok(Box::new(match expr.as_ref() {
            parse::Expr::ArithmeticBinOp(op, l, r) => {
                let l = self.from_expr(l)?;
                let r = self.from_expr(r)?;
                
                if l.dtype.is_int() && r.dtype.is_int() {
                    TypedExpr::new(DataType::Int, Expr::ArithmeticBinOp(*op, l, r))
                }
                else if l.dtype.is_float() && r.dtype.is_float() {
                    TypedExpr::new(DataType::Float, Expr::ArithmeticBinOp(*op, l, r))
                }
                else { return Err(CompileError::TypeMismatch(
                    format!("Cannot perform binary operation `{}` on operands of types `{}` and `{}`", op, l.dtype.to_string(), r.dtype.to_string())));
                }
            },

            parse::Expr::Variable(ident) => self.from_expr_Variable(ident)?,

            parse::Expr::Int(val) => TypedExpr::new(DataType::Int, Expr::Literal(Literal::Int(*val))),

            parse::Expr::Bool(val) => TypedExpr::new(DataType::Bool, Expr::Literal(Literal::Bool(*val))),

            parse::Expr::Float(val) => TypedExpr::new(DataType::Float, Expr::Literal(Literal::Float(*val))),

            parse::Expr::Nil => TypedExpr::new(DataType::Nil, Expr::Literal(Literal::Nil)),

            parse::Expr::Error => panic!("Parse Tree with errors should not have made it to conversion to AST."),
        }))
    }

    fn from_expr_Variable(&mut self, ident: &String) -> Result<TypedExpr, CompileError> {
        match self.symbols.get_variable(ident) {
            Some(&dtype) => {
                Ok(TypedExpr { dtype, expr: Expr::Variable(ident.to_string()) })
            },
            None => Err(CompileError::UndefinedVariable(
                format!("Use of undeclared variable `{ident}`")
            )),
        }
    }

    pub fn unwrap(self) -> Vec<Box<Stmt>> { self.stmts }

    pub fn from_parse_tree(tree: ParseTree) -> Result<Self, CompileError> {
        let mut ast = Self {
            stmts: Vec::new(),
            symbols: SymbolTable::new(),
        };

        for stmt in tree.unwrap() {
            let stmt = ast.from_stmt(&stmt)?;
            ast.stmts.push(stmt);
        }

        Ok(ast)
    }
}