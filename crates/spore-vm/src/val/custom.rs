use std::any::{type_name_of_val, Any};

use super::Val;

pub trait SporeCustomType: 'static + std::fmt::Debug {
    fn spore_as_any<'a>(&'a self) -> &'a dyn Any;
    fn spore_as_any_mut<'a>(&'a mut self) -> &'a mut dyn Any;
    fn spore_name(&self) -> &'static str {
        type_name_of_val(self)
    }
    fn spore_debug_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt(f)
    }
}

pub struct SporeCustom {
    value: Box<dyn SporeCustomType>,
}

impl SporeCustom {
    pub fn new(value: impl SporeCustomType) -> SporeCustom {
        SporeCustom {
            value: Box::new(value),
        }
    }

    pub fn name(&self) -> &str {
        self.value.spore_name()
    }

    pub fn get(&self) -> &dyn Any {
        self.value.spore_as_any()
    }

    pub fn get_mut(&mut self) -> &mut dyn Any {
        self.value.spore_as_any_mut()
    }

    pub fn references(&self) -> &[Val] {
        // TODO: Allow types to store references.
        &[]
    }
}

impl std::fmt::Debug for SporeCustom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.spore_debug_fmt(f)
    }
}
