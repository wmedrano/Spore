use crate::val::{symbol::SymbolId, Val};

#[derive(Clone, Debug, PartialEq)]
/// Represents a single instruction in the bytecode.
pub enum Instruction {
    /// Pushes a value onto the stack.
    Push(Val),
    /// Evaluates a function call.
    Eval(usize),
    /// Gets a value from the stack.
    Get(usize),
    /// Dereferences a symbol.
    Deref(SymbolId),
    /// Returns from a function.
    Return,
}
