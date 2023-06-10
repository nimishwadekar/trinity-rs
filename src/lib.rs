use codegen::CodeGenerator;
use lexer::Lexer;
use parser::Parser;
use vm::TrinityVM;

mod lexer;
mod parser;
mod bytecode;
mod codegen;
mod vm;

pub type CompilerResult<T> = Result<T, String>;

//======================================================================================
//          CONSTANTS
//======================================================================================

pub const EOF: char = '\0';

//======================================================================================
//          ENUMERATIONS
//======================================================================================

pub enum OutputStage {
    Lex,
    Parse,
    Code,
    ExecuteTrace,
    Execute,
}

//======================================================================================
//          FUNCTIONS
//======================================================================================

pub fn compile_and_run(src: String, output: &mut impl std::io::Write, arg: OutputStage) -> CompilerResult<()> {
    let lexer = Lexer::new(src);

    if let OutputStage::Lex = arg {
        lexer.print_tokens();
        return Ok(());
    }

    let (parsed_program, symbol_table) = Parser::parse(lexer.iter())?;
    if let OutputStage::Parse = arg {
        println!("{}", parsed_program);
        return Ok(());
    }
    
    let code = CodeGenerator::generate(parsed_program, symbol_table)?;
    if let OutputStage::Code = arg {
        println!("{}", code);
        return Ok(());
    }

    let trace = if let OutputStage::ExecuteTrace = arg { true } else { false };
    
    TrinityVM::execute(code, trace, output)?;

    Ok(())
}