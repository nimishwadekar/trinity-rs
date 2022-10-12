mod instruction;
mod linkable;
mod executable;

pub use instruction::Instruction;
pub use linkable::LinkableByteCode;
pub use executable::ExecutableByteCode;

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! debug {
    ($x:expr) => { dbg!($x) }
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! debug {
    ($x:expr) => { std::convert::identity($x) }
}