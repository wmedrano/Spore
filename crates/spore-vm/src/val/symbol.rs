use std::collections::HashMap;

use compact_str::CompactString;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
/// An identifier for a symbol.
pub struct SymbolId(u32);

impl SymbolId {
    /// Get the symbol id as a number.
    pub fn as_num(self) -> u32 {
        self.0
    }
}

#[derive(Default)]
/// A table mapping symbols to names.
pub struct SymbolTable {
    idx_to_name: Vec<CompactString>,
    name_to_id: HashMap<CompactString, SymbolId>,
}

impl SymbolTable {
    /// Returns the symbol ID for a given name, creating a new one if it doesn't exist.
    pub fn make_symbol_id(&mut self, name: &str) -> SymbolId {
        match self.name_to_id.get(name) {
            Some(id) => *id,
            None => {
                let id = SymbolId(self.idx_to_name.len() as u32);
                self.idx_to_name.push(CompactString::new(name));
                self.name_to_id.insert(CompactString::new(name), id);
                id
            }
        }
    }

    /// Returns the symbol ID for a given name or `None` if it doesn't exist.
    pub fn symbol_id(&self, name: &str) -> Option<SymbolId> {
        self.name_to_id.get(name).copied()
    }

    /// Returns the name for a given symbol ID.
    pub fn symbol_name(&self, id: SymbolId) -> Option<&str> {
        self.idx_to_name
            .get(id.0 as usize)
            .map(CompactString::as_str)
    }
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.idx_to_name.fmt(f)
    }
}
