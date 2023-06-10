mod common;

mod expressions {
    use crate::common::test_output;

    #[test]
    fn basic() {
        test_output("print 1;", "1\n");
    }

    #[test]
    fn arithmetic() {
        test_output("print 10 / 3 + 15 * 4 - 7 % 5 * 2; print 10.0 / 4.0;", "59\n2.5\n");
    }

    #[test]
    fn relational() {
        test_output("print 1 > -4; print 3.3 == 3.3; print true != true;", "true\ntrue\nfalse\n");
    }

    #[test]
    fn logical() {
        test_output("print true and false; print 2 * 3 == 5 or not (2 * 0 <= -1);", "false\ntrue\n");
    }

    #[test]
    #[should_panic]
    fn int_float_arithmetic_mismatch() {
        test_output("print 1 + 2.0", "");
    }
}