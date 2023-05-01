use std::rc::Rc;

use lexer::Lexer;

mod lexer;

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

fn parse_arguments() -> OutputStage {
    let mut args = std::env::args();
    args.next();
    
    let arg = match args.next() {
        Some(arg) => match arg.as_str() {
            "--lex" => OutputStage::Lex,
            "--parse" => OutputStage::Parse,
            "--code" => OutputStage::Code,
            arg => panic!("Invalid argument: {}", arg),
        },
        None => OutputStage::Execute,
    };
    if let Some(..) = args.next() {
        panic!("Only one argument expected");
    }

    arg
}

fn main() {
    println!("\n=================================================================================================================\n");

    let arg = parse_arguments();

    let mut src = match std::fs::read_to_string("test.neo") {
        Ok(src) => src,
        Err(e) => panic!("File read failed: {}", e),
    };
    // Push EOF character.
    src.push('\0');
    println!("{:?}\n", src);

    let src = Rc::new(src);

    let lexer = Lexer::new(src.clone());
    
    for token in lexer.iter() {
        println!("{:?}", token);
    }

    println!("\n=================================================================================================================\n");
}
