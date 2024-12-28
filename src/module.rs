use std::collections::HashMap;

use crate::val::{symbol::SymbolId, Val};

pub struct Module {
    pub values: HashMap<SymbolId, Val>,
}

impl Module {
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
