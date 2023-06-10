mod common;

mod let_stmt {
    use crate::common::test_output;

    #[test]
    fn basic() {
        test_output("let x: int = 5;
        let y: int = 1;
        let z: int = x + y;
        print z + 5 == 11;", "true\n");
    }
}