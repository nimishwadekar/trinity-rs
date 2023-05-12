use std::rc::Rc;

use codegen::CodeGenerator;
use lexer::Lexer;
use parser::Parser;
use vm::TrinityVM;

mod lexer;
mod parser;
mod bytecode;
mod codegen;
mod vm;

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
    ExecuteTrace,
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
            "--trace" => OutputStage::ExecuteTrace,
            "--execute" => OutputStage::Execute,
            arg => return Err(format!("Invalid argument `{}`", arg)),
        },
        None => OutputStage::Execute,
    };
    if let Some(..) = args.next() {
        return Err("Only one argument expected".to_string());
    }

    Ok(arg)
}

fn usage() -> &'static str {
    "Usage:
    Arguments:
    <none> or `--execute` - Executes the program normally.
    `--lex` - Displays the tokenised version of the source code.
    `--parse` - Displays the syntax tree created after parsing.
    `--code` - Displays the byte code generated.
    `--trace` - Executes the program but displays every byte code instruction as it executes."
}

fn driver() -> Result<(), String> {
    let arg = match parse_arguments() {
        Ok(arg) => arg,
        Err(e) => return Err(format!("{}\n\n{}", e, usage())),
    };

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
    let parsed_program = parser.parse()?;
    if let OutputStage::Parse = arg {
        println!("{}", parsed_program);
        return Ok(());
    }
    
    let code = CodeGenerator::generate(parsed_program)?;
    if let OutputStage::Code = arg {
        println!("{}", code);
        return Ok(());
    }

    let trace = if let OutputStage::ExecuteTrace = arg { true } else { false };
    
    TrinityVM::execute(code, trace)?;

    Ok(())
}

fn main() {
    println!("\n=================================================================================================================\n");

    if let Err(e) = driver() {
        println!("\x1b[91mERROR\x1b[0m: {}", e);
    }

    println!("\n=================================================================================================================\n");
}
