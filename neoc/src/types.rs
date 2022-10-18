#[derive(PartialEq, Eq, Clone, Copy)]
pub enum DataType {
    Int, // i64
    Bool,
    Float, // f64

    /// Equivalent to `()` in Rust, not `null` in Java.
    Nil,
}

impl DataType {
    pub fn is_int(&self) -> bool {
        *self == DataType::Int
    }

    pub fn is_bool(&self) -> bool {
        *self == DataType::Bool
    }

    pub fn is_float(&self) -> bool {
        *self == DataType::Float
    }

    pub fn is_nil(&self) -> bool {
        *self == DataType::Nil
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            Self::Int => "int",
            Self::Bool => "bool",
            Self::Float => "float",
            Self::Nil => "nil",
        }
    }
}