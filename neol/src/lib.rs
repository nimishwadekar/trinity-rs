use error::LinkingError;
use link::ByteCode;
use neo_util::{LinkableByteCode, ExecutableByteCode};

mod link;
mod error;

pub fn link(links: Vec<LinkableByteCode>) -> Result<ExecutableByteCode, LinkingError> {
    ByteCode::link(links)
}
