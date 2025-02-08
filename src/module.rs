use std::collections::HashMap;

use crate::val::{symbol::SymbolId, Val};

#[derive(Debug)]
/// A module containing named values.
pub struct Module {
    /// The named values in this module.
    pub values: HashMap<SymbolId, Val>,
}

impl Module {
    /// Creates a new, empty module.
    pub fn new() -> Module {
        Module {
            values: HashMap::new(),
        }
    }
}

impl Default for Module {
    fn default() -> Self {
        Module::new()
    }
}
