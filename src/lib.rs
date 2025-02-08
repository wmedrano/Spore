use std::rc::Rc;

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
