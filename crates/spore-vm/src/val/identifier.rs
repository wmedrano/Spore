use std::collections::HashMap;

use compact_str::CompactString;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
/// A unique identifier.
pub struct IdentifierId(u32);

impl IdentifierId {
    /// Get the identifier as a number.
    pub fn as_num(self) -> u32 {
        self.0
    }
}

#[derive(Default)]
/// A table to convert between identifiers and their identifier id.
pub struct IdentifierTable {
    idx_to_name: Vec<CompactString>,
    name_to_id: HashMap<CompactString, IdentifierId>,
}

impl IdentifierTable {
    /// Returns the symbol ID for a given name, creating a new one if it doesn't exist.
    pub fn make_identifier_id(&mut self, name: &str) -> IdentifierId {
        match self.name_to_id.get(name) {
            Some(id) => *id,
            None => {
                let id = IdentifierId(self.idx_to_name.len() as u32);
                self.idx_to_name.push(CompactString::new(name));
                self.name_to_id.insert(CompactString::new(name), id);
                id
            }
        }
    }

    /// Returns the symbol ID for a given name or `None` if it doesn't exist.
    pub fn identifier_id(&self, name: &str) -> Option<IdentifierId> {
        self.name_to_id.get(name).copied()
    }

    /// Returns the name for a given symbol ID.
    pub fn identifier(&self, id: IdentifierId) -> Option<&str> {
        self.idx_to_name
            .get(id.0 as usize)
            .map(CompactString::as_str)
    }
}

impl std::fmt::Debug for IdentifierTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.idx_to_name.fmt(f)
    }
}
