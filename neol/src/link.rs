use neo_util::{
    LinkableByteCode,
    ExecutableByteCode,
};
use crate::error::LinkingError;

//=========================================
// TYPES
//=========================================

pub struct ByteCode(ExecutableByteCode);

//=========================================
// IMPLEMENTATIONS
//=========================================

impl ByteCode {
    pub fn link(links: Vec<LinkableByteCode>) -> Result<ExecutableByteCode, LinkingError> {
        if links.len() > 1 { panic!("Only one link supported right now."); }

        let LinkableByteCode { code, constants } = links.into_iter().next().unwrap();
        Ok(ExecutableByteCode {
            code,
            constants,
        })
    }
}