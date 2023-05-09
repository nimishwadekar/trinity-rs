use std::rc::Rc;

use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;

//======================================================================================
//          CONSTANTS
//======================================================================================

pub const EOF: char = '\0';

//======================================================================================
//          STRUCTURES
//======================================================================================

enum OutputStage {
    Lex,
    Parse,
    Code,
    Execute,
}

//======================================================================================
//          FUNCTIONS
//======================================================================================

fn parse_arguments() -> Result<OutputStage, String> {
    let mut args = std::env::args();
    args.next();
    
    let arg = match args.next() {
        Some(arg) => match arg.as_str() {
            "--lex" => OutputStage::Lex,
            "--parse" => OutputStage::Parse,
            "--code" => OutputStage::Code,
            arg => return Err(format!("Invalid argument `{}`", arg)),
        },
        None => OutputStage::Execute,
    };
    if let Some(..) = args.next() {
        return Err("Only one argument expected".to_string());
    }

    Ok(arg)
}

fn driver() -> Result<(), String> {
    let arg = parse_arguments()?;

    let src = match std::fs::read_to_string("test.neo") {
        Ok(mut src) => {
            src.push(EOF);
            Rc::new(src)
        },
        Err(e) => return Err(e.to_string()),
    };

    let lexer = Lexer::new(src.clone());

    if let OutputStage::Lex = arg {
        lexer.print_tokens();
        return Ok(());
    }

    let mut parser = Parser::new(lexer.iter());
    parser.parse();

    Ok(())
}

fn main() {
    println!("\n=================================================================================================================\n");

    if let Err(e) = driver() {
        println!("\x1b[91mERROR\x1b[0m: {}", e);
    }

    println!("\n=================================================================================================================\n");
}
