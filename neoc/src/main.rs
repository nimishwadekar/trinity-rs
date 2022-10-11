use neoc;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Only one argument (file to compile) expected!");
    }
    
    let source = fs::read_to_string(&args[1]).unwrap();
    let parse_result = neoc::compile(&source);

    match parse_result.errors {
        Some(errors) => {
            eprintln!("********** PARSER ERRORS **********");
            eprintln!("AST = {:#?}", parse_result.ast);
            eprintln!("Errors = {:#?}", errors);
        },
        None => {
            eprintln!("AST = {:#?}", parse_result.ast);
        }
    }
}