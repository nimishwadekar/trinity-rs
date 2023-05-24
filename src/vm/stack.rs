use crate::CompilerResult;

//======================================================================================
//          CONSTANTS
//======================================================================================

const MAX_STACK_ENTRIES: usize = 1 << 17;

//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================

#[derive(Debug)]
pub enum Value {
    Int(i64),
}

pub struct Stack {
    stack: Vec<Value>,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.stack.iter().peekable();
        write!(f, "[")?;
        while let Some(value) = iter.next() {
            write!(f, "{:?}", value)?;
            if let Some(..) = iter.peek() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl Stack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn push(&mut self, value: Value) -> CompilerResult<()> {
        if self.stack.len() >= MAX_STACK_ENTRIES {
            return Err("Stack overflow".to_string());
        }
        self.stack.push(value);
        Ok(())
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("Empty stack pop")
    }
}

impl Value {
    pub fn as_int(self) -> i64 {
        match self {
            Value::Int(val) => val,
            _ => panic!("Expected `Value::Int`, found {:?}", self),
        }
    }
}