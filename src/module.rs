use std::collections::HashMap;

use crate::val::{symbol::SymbolId, Val};

#[derive(Debug)]
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
