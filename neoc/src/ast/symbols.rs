use std::collections::HashMap;

use crate::types::DataType;

//=========================================
// TYPES
//=========================================

#[derive(Debug)]
pub struct SymbolTable {
    variables: HashMap<String, DataType>,
}

//=========================================
// IMPLEMENTATIONS
//=========================================

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get_variable(&mut self, ident: &String) -> Option<&DataType> {
        self.variables.get(ident)
    }

    pub fn add_variable(&mut self, ident: String, dtype: DataType) -> Option<DataType> {
        self.variables.insert(ident, dtype)
    }
}