pub mod functions;
pub mod symbol;

use functions::{ByteCodeFunction, NativeFunction};
use symbol::SymbolId;

use crate::{object_store::ObjectId, vm::Vm};

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents a value in the VM.
pub enum Val {
    /// Represents the lack of a value.
    Void,
    /// Represents a boolean value.
    Bool(bool),
    /// Represents an integer.
    Int(i64),
    /// Represents a floating-point number.
    Float(f64),
    /// Represents a symbol.
    Symbol(SymbolId),
    /// Represents a native function.
    NativeFunction(ObjectId<NativeFunction>),
    /// Represents a bytecode function.
    BytecodeFunction(ObjectId<ByteCodeFunction>),
}

impl Val {
    /// Creates a formatter for the value.
    pub fn formatter(self, vm: &Vm) -> ValFormatter {
        ValFormatter { vm, val: self }
    }
}

/// A formatter for values that uses the VM to resolve symbols.
pub struct ValFormatter<'a> {
    vm: &'a Vm,
    val: Val,
}

impl std::fmt::Debug for ValFormatter<'_> {
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

impl std::fmt::Display for ValFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.val {
            Val::Void => write!(f, "<void>"),
            Val::Bool(x) => write!(f, "{x}"),
            Val::Int(x) => write!(f, "{x}"),
            Val::Float(x) => write!(f, "{x}"),
            Val::Symbol(symbol_id) => match self.vm.symbol_name(symbol_id) {
                Some(x) => write!(f, "'{x}"),
                None => write!(f, "'<symbol-{}>", symbol_id.as_num()),
            },
            Val::NativeFunction(object_id) => write!(f, "<nativefunction-{}>", object_id.as_num()),
            Val::BytecodeFunction(object_id) => write!(f, "<function-{}>", object_id.as_num()),
        }
    }
}
