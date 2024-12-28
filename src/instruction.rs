use crate::val::{symbol::SymbolId, Val};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Push(Val),
    Eval(usize),
    Deref(SymbolId),
    Return,
}
