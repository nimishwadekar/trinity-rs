use neoc;
use std::{env, fs};
use neo_util::debug;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Only one argument (file to compile) expected!");
    }
    
    let source = fs::read_to_string(&args[1]).unwrap();
    debug!(neoc::compile(&source).unwrap());
}