use std::collections::HashMap;

use compact_str::CompactString;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
/// An identifier for a symbol.
pub struct SymbolId(u32);

#[derive(Default)]
/// A table mapping symbols to names.
pub struct SymbolTable {
    id_to_name: HashMap<SymbolId, CompactString>,
    name_to_id: HashMap<CompactString, SymbolId>,
    next_id: u32,
}

impl SymbolTable {
    /// Returns the symbol ID for a given name, creating a new one if it doesn't exist.
    pub fn symbol_id(&mut self, name: &str) -> SymbolId {
        match self.name_to_id.get(name) {
            Some(id) => *id,
            None => {
                let id = SymbolId(self.next_id);
                self.next_id += 1;
                self.id_to_name.insert(id, CompactString::new(name));
                self.name_to_id.insert(CompactString::new(name), id);
                id
            }
        }
    }

    /// Returns the name for a given symbol ID.
    pub fn symbol_name(&self, id: SymbolId) -> Option<&str> {
        self.id_to_name.get(&id).map(CompactString::as_str)
    }
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id_to_name.fmt(f)
    }
}
