use std::collections::HashMap;

use compact_str::CompactString;

use crate::val::Val;

pub struct Module {
    pub symbols: HashMap<CompactString, Val>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            symbols: HashMap::new(),
        }
    }
}

impl Default for Module {
    fn default() -> Self {
        Module::new()
    }
}
