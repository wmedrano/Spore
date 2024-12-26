use std::collections::HashMap;

use compact_str::CompactString;

use crate::val::Val;

pub struct Module {
    pub symbols: HashMap<CompactString, Val>,
}
