use std::fmt::Debug;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum DataType {
    Int, // i64
    Bool,
    Float, // f64

    /// Equivalent to `()` in Rust, not `null` in Java.
    Nil,
}

impl Debug for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_string())
    }
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

    pub fn try_primitive_from(dtype: &str) -> Option<Self> {
        match dtype {
            "int" => Some(Self::Int),
            "bool" => Some(Self::Bool),
            "float" => Some(Self::Float),
            "nil" => Some(Self::Nil),
            _ => None,
        }
    }
}