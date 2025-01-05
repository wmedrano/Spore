pub mod functions;
pub mod symbol;

use functions::{ByteCodeFunction, NativeFunction};
use symbol::SymbolId;

use crate::{object_store::ObjectId, vm::Vm};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Val {
    Void,
    Int(i64),
    Float(f64),
    Symbol(SymbolId),
    NativeFunction(ObjectId<NativeFunction>),
    BytecodeFunction(ObjectId<ByteCodeFunction>),
}

impl Val {
    pub fn formatter(self, vm: &Vm) -> ValFormatter {
        ValFormatter { vm, val: self }
    }
}

pub struct ValFormatter<'a> {
    vm: &'a Vm,
    val: Val,
}

impl<'a> std::fmt::Debug for ValFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.val {
            Val::Symbol(symbol_id) => match self.vm.symbol_name(symbol_id) {
                Some(symbol_name) => f.debug_tuple("Symbol").field(&symbol_name).finish(),
                None => f
                    .debug_tuple("Symbol")
                    .field(&"_")
                    .field(&symbol_id)
                    .finish(),
            },
            v => v.fmt(f),
        }
    }
}
