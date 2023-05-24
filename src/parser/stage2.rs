use crate::CompilerResult;
use crate::lexer::Lexeme;
use crate::parser::stage1::{ParseTreeV1, StmtV1, StmtTypeV1, ExprV1, ExprTypeV1};

//======================================================================================
//          CONSTANTS
//======================================================================================



//======================================================================================
//          MACROS
//======================================================================================

macro_rules! is_type_eq {
    ($a:expr, $b:expr) => {
        $a.data_type == $b.data_type
    };
}

macro_rules! is_int {
    ($e:expr) => {
        $e.data_type == DataType::Int
    };
}

//======================================================================================
//          STRUCTURES
//======================================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    Int,
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
        self.display_format(f, 0)
    }
}

impl std::fmt::Display for ExprV2 {
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
                
                let data_type = if is_int!(l) && is_int!(r) {
                    l.data_type
                } else {
                    return Err(err_format("Invalid operand data type for `+`", l.lexeme()));
                };

                (ExprTypeV2::Add { l, r }, data_type)
            },

            ExprTypeV1::Positive(expr) => {
                let expr = self.parse_expr(expr)?;
                
                let data_type = if is_int!(expr) {
                    expr.data_type
                } else {
                    return Err(err_format("Invalid operand data type for `+`", expr.lexeme()));
                };

                (ExprTypeV2::Positive(expr), data_type)
            }
        };

        Ok(Box::new(ExprV2::new(expr_type, data_type, expr.lexeme().clone())))
    }

    fn get_integer_type(&self, lexeme: &Lexeme) -> CompilerResult<DataType> {
        if let Ok(..) = lexeme.parse::<i64>() {
            return Ok(DataType::Int);
        }
        
        Err(err_format("Integer too large", lexeme))
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
}

impl ExprV2 {
    fn new(expr: ExprTypeV2, data_type: DataType, lexeme: Lexeme) -> Self {
        Self { expr, data_type, lexeme }
    }

    pub fn expr(&self) -> &ExprTypeV2 {
        &self.expr
    }

    pub fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
}

//=======================================
//          FORMAT METHODS
//=======================================

const NEXT_INDENT: usize = 3;

impl StmtV2 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}", "")?;
        let indent = indent + NEXT_INDENT;
        match self.stmt() {
            StmtTypeV2::Expr(expr) => {
                writeln!(f, "ExprStmt")?;
                expr.display_format(f, indent)?;
            },
            StmtTypeV2::Print(expr) => {
                writeln!(f, "PrintStmt")?;
                expr.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

impl ExprV2 {
    fn display_format(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$} <{}> ", "", self.data_type)?;
        let indent = indent + NEXT_INDENT;
        match self.expr() {
            ExprTypeV2::IntegerLiteral(value) => writeln!(f, "Integer {}", value)?,
            ExprTypeV2::Positive(expr) => {
                writeln!(f, "Positive")?;
                expr.display_format(f, indent)?;
            },
            ExprTypeV2::Add { l, r } => {
                writeln!(f, "Add")?;
                l.display_format(f, indent)?;
                r.display_format(f, indent)?;
            },
        };
        Ok(())
    }
}

#[inline(always)]
fn err_format(err: &str, lexeme: &Lexeme) -> String {
    format!("<{}> `{}`: {}", lexeme.location(), lexeme, err)
}