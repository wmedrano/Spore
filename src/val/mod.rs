pub mod functions;
pub mod symbol;

use functions::{ByteCodeFunction, NativeFunction};
use symbol::SymbolId;

use crate::object_store::ObjectId;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Void,
    Int(i64),
    Float(f64),
    Symbol(SymbolId),
    NativeFunction(ObjectId<NativeFunction>),
    BytecodeFunction(ObjectId<ByteCodeFunction>),
}
