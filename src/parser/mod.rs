use crate::lexer::TokenStream;

use self::stage1::{Stage1Tree, Stage1Parser};



pub mod stage1;
mod stage2;

pub struct Parser;

impl Parser {
    pub fn parse<'a>(tokens: TokenStream<'a>)-> Result<Stage1Tree, String> {
        let mut parser = Stage1Parser::new(tokens);
        parser.parse()
    }
}