use std::{collections::HashMap, rc::Rc};

use val::{symbol::SymbolId, Val};

/// Builtin functions.
pub mod builtins;
/// Compiler.
pub mod compiler;
/// Errors
pub mod error;
/// Items related to garbage collection and object management.
pub mod gc;
/// Instructions.
pub mod instruction;
/// Modules.
pub mod module;
/// Values.
pub mod val;
/// Virtual machine.
pub mod vm;

/// A reference counted pointer.
pub type SporeRc<T> = Rc<T>;

/// The physical struct representation.
pub type SporeStruct = HashMap<SymbolId, Val>;

/// The physical list representation.
pub type SporeList = Vec<Val>;

pub use val::custom::SporeCustomType;

#[macro_export]
macro_rules! register_spore_type {
    ($typename:ty) => {
        register_spore_type!($typename, &[]);
    };
    ($typename:ty, $references:expr) => {
        impl $crate::SporeCustomType for $typename {
            fn spore_as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn spore_as_any_mut(&mut self) -> &mut dyn std::any::Any {
                self
            }
        }
    };
}
