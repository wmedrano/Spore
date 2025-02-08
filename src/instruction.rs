use crate::val::{symbol::SymbolId, Val};

#[derive(Clone, Debug, PartialEq)]
/// Represents a single instruction in the bytecode.
pub enum Instruction {
    Push(Val),
    Eval(usize),
    Get(usize),
    Deref(SymbolId),
    Return,
}
