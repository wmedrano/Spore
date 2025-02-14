use std::{collections::HashMap, rc::Rc};

use compact_str::CompactString;
use val::{symbol::SymbolId, Val};

/// Builtin functions.
pub mod builtins;
/// Compiler.
pub mod compiler;
/// Instructions.
pub mod instruction;
/// Modules.
pub mod module;
/// Object store.
pub mod object_store;
/// Values.
pub mod val;
/// Virtual machine.
pub mod vm;

/// A reference counted pointer.
pub type SporeRc<T> = Rc<T>;

/// The physical string representation.
pub type SporeString = CompactString;

/// The physical struct representation.
pub type SporeStruct = HashMap<SymbolId, Val>;

/// The physical list representation.
pub type SporeList = Vec<Val>;
