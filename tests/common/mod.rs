use std::str::from_utf8;

use trinity_rs::{EOF, compile_and_run};

pub fn test_output(src: &str, expected: &str) {
    let mut src = src.to_string();
    src.push(EOF);
    let mut output = Vec::new();
    compile_and_run(src, &mut output, trinity_rs::OutputStage::Execute).unwrap();
    assert_eq!(from_utf8(&output).unwrap(), expected);
}