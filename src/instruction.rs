use compact_str::CompactString;

use crate::val::Val;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Push(Val),
    Eval(usize),
    Deref(CompactString),
    Return,
}
