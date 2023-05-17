use crate::{lexer::TokenStream, CompilerResult};

use self::{
    stage1::ParserV1,
    stage2::{ParseTreeV2, ParserV2}
};

mod stage1;
mod stage2;

pub use stage2::ParseTreeV2 as ParseTree;
pub use stage2::ExprV2 as Expr;
pub use stage2::ExprTypeV2 as ExprType;
pub use stage2::StmtV2 as Stmt;
pub use stage2::StmtTypeV2 as StmtType;

pub struct Parser;

impl Parser {
    pub fn parse<'a>(tokens: TokenStream<'a>)-> CompilerResult<ParseTreeV2> {
        let tree = ParserV1::parse(tokens)?;
        ParserV2::parse(tree)
    }
}