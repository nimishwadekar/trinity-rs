use std::{env, fs};

use neoc;
use neol;
use trivm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Only one argument (file to compile) expected!");
    }
    let source = fs::read_to_string(&args[1]).unwrap();

    let code = neoc::compile(&source).unwrap();
    let code = neol::link(vec![code]).unwrap();
    trivm::execute(code).unwrap();
}
