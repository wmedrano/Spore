use std::collections::HashMap;

use crate::val::{Val, identifier::IdentifierId};

#[derive(Debug)]
/// A module containing named values.
pub struct Module {
    /// The named values in this module.
    pub values: HashMap<IdentifierId, Val>,
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
