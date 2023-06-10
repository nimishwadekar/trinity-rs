use trinity_rs::{
    CompilerResult,
    OutputStage, EOF, compile_and_run,
};

//======================================================================================
//          FUNCTIONS
//======================================================================================

fn parse_arguments() -> CompilerResult<OutputStage> {
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

fn driver() -> CompilerResult<()> {
    let arg = match parse_arguments() {
        Ok(arg) => arg,
        Err(e) => return Err(format!("{}\n\n{}", e, usage())),
    };

    let src = match std::fs::read_to_string("test.neo") {
        Ok(mut src) => {
            src.push(EOF);
            src
        },
        Err(e) => return Err(e.to_string()),
    };

    compile_and_run(src, &mut std::io::stdout(), arg)
}

fn main() {
    println!("\n=================================================================================================================\n");

    if let Err(e) = driver() {
        println!("\x1b[91mERROR\x1b[0m: {}", e);
    }

    println!("\n=================================================================================================================\n");
}
