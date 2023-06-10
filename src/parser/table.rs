use std::collections::HashMap;
use crate::{lexer::Lexeme, CompilerResult};
use super::DataType;

//======================================================================================
//          CONSTANTS
//======================================================================================

const MAX_SYMBOLS_IN_SCOPE: usize = u8::MAX as usize + 1;

//======================================================================================
//          MACROS
//======================================================================================



//======================================================================================
//          STRUCTURES
//======================================================================================

// Try vector of queues
#[derive(Debug)]
pub struct SymbolTable {
    table: HashMap<Lexeme, SymbolTableEntry>,
    max_identifiers_in_scope: usize,
}

#[derive(Debug)]
pub struct SymbolTableEntry {
    pub dtype: DataType,
    pub index: u8,
}

//======================================================================================
//          STANDARD LIBRARY TRAIT IMPLEMENTATIONS
//======================================================================================

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (identifier, SymbolTableEntry { dtype, index }) in self.table.iter() {
            writeln!(f, "{identifier} <{dtype}> : {index}")?;
        }
        Ok(())
    }
}

//======================================================================================
//          IMPLEMENTATIONS
//======================================================================================

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            max_identifiers_in_scope: 0,
        }
    }

    pub fn max_identifiers_in_scope(&self) -> usize {
        self.max_identifiers_in_scope
    }

    pub fn insert(&mut self, identifier: Lexeme, dtype: DataType) -> CompilerResult<()> {
        let index = self.table.len();
        if index == MAX_SYMBOLS_IN_SCOPE {
            return Err("Too many variables in the current scope".to_string());
        }
        self.table.insert(identifier, SymbolTableEntry::new(dtype, index as u8));
        self.max_identifiers_in_scope += 1;
        Ok(())
    }

    pub fn get(&self, identifier: &Lexeme) -> Option<&SymbolTableEntry> {
        self.table.get(identifier)
    }
}

impl SymbolTableEntry {
    fn new(dtype: DataType, index: u8) -> Self {
        Self { dtype, index }
    }
}