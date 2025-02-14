use std::rc::Rc;

use compact_str::CompactString;
use val::Val;

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

pub type SporeList = Vec<Val>;
