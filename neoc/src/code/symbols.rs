use std::collections::HashMap;

//=========================================
// TYPES
//=========================================

#[derive(Debug)]
pub struct SymbolTable {
    variables: HashMap<String, u8>,
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

    pub fn get_variable(&mut self, ident: &String) -> u8 {
        *self.variables.get(ident).expect("Should not have reached code generation symbol table")
    }

    pub fn add_variable(&mut self, ident: String, offset: u8) {
        self.variables.insert(ident, offset);
    }
}