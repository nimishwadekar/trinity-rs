use neoc;
use std::{env, fs, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Only one argument (file to compile) expected!");
    }
    
    let source = fs::read_to_string(&args[1]).unwrap();
    let output = match neoc::compile(&source) {
        Ok(output) => output,
        Err(e) => {
            eprintln!("ERROR: {}", e);
            process::exit(1);
        }
    };

    dbg!(output);
}