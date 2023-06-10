use std::collections::{HashMap, VecDeque, hash_map::Entry};
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
    tables: Vec<VecDeque<HashMap<Lexeme, SymbolTableEntry>>>,
    current_depth: usize,
    current_identifier_count: usize,
    max_identifiers_in_scope: usize,

    /// Lock the table after parsing has completely filled the table and it will only be read afterwards.
    is_locked: bool,
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
        for (depth, tables) in self.tables.iter().enumerate() {
            writeln!(f, "Depth {depth}")?;
            for table in tables {
                for (identifier, SymbolTableEntry { dtype, index }) in table {
                    writeln!(f, "{identifier} <{dtype}> : {index}")?;
                }
                writeln!(f)?;
            }
            writeln!(f, "---------------------")?;
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
            tables: vec![VecDeque::from([HashMap::new()])],
            current_depth: 0,
            current_identifier_count: 0,
            max_identifiers_in_scope: 0,
            is_locked: false,
        }
    }

    pub fn lock(&mut self) {
        self.is_locked = true;
    }

    pub fn max_identifiers_in_scope(&self) -> usize {
        self.max_identifiers_in_scope
    }

    pub fn open_scope(&mut self) {
        self.current_depth += 1;

        if !self.is_locked {
            if self.current_depth == self.tables.len() {
                self.tables.push(VecDeque::new());
            }
            self.tables[self.current_depth].push_back(HashMap::new());
        }
    }

    pub fn close_scope(&mut self) {
        if !self.is_locked {
            self.current_identifier_count -= self.tables[self.current_depth].back().unwrap().len();
        } else {
            self.tables[self.current_depth].pop_front().unwrap(); // Discard table for now. Cycle if reused.
        }
        self.current_depth -= 1;
    }

    /// Only used when unlocked.
    pub fn insert(&mut self, identifier: Lexeme, dtype: DataType) -> CompilerResult<()> {
        assert!(!self.is_locked);

        match self.tables[self.current_depth].back_mut().unwrap().entry(identifier) {
            Entry::Occupied(mut entry) => entry.get_mut().dtype = dtype,
            Entry::Vacant(entry) => {
                if self.current_identifier_count == MAX_SYMBOLS_IN_SCOPE {
                    return Err("Too many variables".to_string());
                }
                entry.insert(SymbolTableEntry::new(dtype, self.current_identifier_count as u8));
                self.current_identifier_count += 1;
                if self.max_identifiers_in_scope < self.current_identifier_count {
                    self.max_identifiers_in_scope = self.current_identifier_count;
                }
            },
        };
        //println!("{}\n==========================", self);
        Ok(())
    }

    pub fn get(&self, identifier: &Lexeme) -> Option<&SymbolTableEntry> {
        for depth in (0..=self.current_depth).rev() {
            let entry = match self.is_locked {
                false => self.tables[depth].back().unwrap().get(identifier),
                true => self.tables[depth].front().unwrap().get(identifier),
            };
            if let e@Some(..) = entry {
                return e;
            }
        }
        None
    }
}

impl SymbolTableEntry {
    fn new(dtype: DataType, index: u8) -> Self {
        Self { dtype, index }
    }
}