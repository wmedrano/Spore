pub mod functions;
pub mod symbol;

use functions::{ByteCodeFunction, NativeFunction};
use symbol::SymbolId;

use crate::{object_store::ObjectId, vm::Vm, SporeList, SporeString, SporeStruct};

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
    /// Contains a string.
    String(ObjectId<SporeString>),
    /// Contains a list of values.
    List(ObjectId<SporeList>),
    /// Contains a map from field to value.
    Struct(ObjectId<SporeStruct>),
    /// Contains a native function.
    NativeFunction(ObjectId<NativeFunction>),
    /// Contains a bytecode function.
    BytecodeFunction(ObjectId<ByteCodeFunction>),
    /// Contains a datatype.
    DataType(DataType),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DataType {
    Void,
    Bool,
    Int,
    Float,
    Symbol,
    String,
    List,
    StructT,
    Function,
    DataType,
}

impl Val {
    /// Creates a formatter for the value.
    pub fn formatted(self, vm: &Vm) -> ValFormatter {
        ValFormatter { vm, val: self }
    }

    /// Returns true if `is_truthy` is not false or void.
    pub fn is_truthy(self) -> bool {
        !matches!(self, Val::Bool(false) | Val::Void)
    }

    pub fn is_void(self) -> bool {
        matches!(self, Val::Void)
    }

    pub fn as_list(self, vm: &Vm) -> Option<&SporeList> {
        match self {
            Val::List(object_id) => vm.objects.get_list(object_id),
            _ => None,
        }
    }

    pub fn as_struct(self, vm: &Vm) -> Option<&SporeStruct> {
        match self {
            Val::Struct(object_id) => vm.objects.get_struct(object_id),
            _ => None,
        }
    }

    pub fn as_symbol_id(self) -> Option<SymbolId> {
        match self {
            Val::Symbol(x) => Some(x),
            _ => None,
        }
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
        let max_depth = 4;
        self.fmt_impl(f, max_depth)
    }
}

impl ValFormatter<'_> {
    fn fmt_impl(&self, f: &mut std::fmt::Formatter<'_>, max_depth: usize) -> std::fmt::Result {
        if max_depth == 0 {
            return write!(f, "..");
        }
        match self.val {
            Val::Void => write!(f, "void"),
            Val::Bool(x) => write!(f, "{x}"),
            Val::Int(x) => write!(f, "{x}"),
            Val::Float(x) => write!(f, "{x}"),
            Val::Symbol(symbol_id) => match self.vm.symbol_name(symbol_id) {
                Some(x) => write!(f, "'{x}"),
                None => write!(f, "'(symbol-{})", symbol_id.as_num()),
            },
            Val::String(string_id) => match self.vm.objects.strings.get(string_id) {
                Some(x) => write!(f, "{x}"),
                None => write!(f, "(gc-string-{})", string_id.as_num()),
            },
            Val::List(list_id) => match self.vm.objects.lists.get(list_id) {
                Some(lst) => {
                    write!(f, "(")?;
                    for (idx, v) in lst.iter().enumerate() {
                        if idx > 0 {
                            write!(f, " ")?;
                        }
                        v.formatted(self.vm).fmt_impl(f, max_depth - 1)?;
                    }
                    write!(f, ")")
                }
                None => write!(f, "(gc-list-{})", list_id.as_num()),
            },
            Val::Struct(struct_id) => match self.vm.objects.structs.get(struct_id) {
                Some(lst) => {
                    write!(f, "(struct ")?;
                    for (idx, (sym, v)) in lst.iter().enumerate() {
                        let sym = Val::Symbol(*sym);
                        if idx > 0 {
                            write!(f, " ")?;
                        }
                        sym.formatted(self.vm).fmt_impl(f, max_depth - 1)?;
                        write!(f, " ")?;
                        v.formatted(self.vm).fmt_impl(f, max_depth - 1)?;
                    }
                    write!(f, ")")
                }
                None => write!(f, "(gc-list-{})", struct_id.as_num()),
            },
            Val::NativeFunction(object_id) => {
                match self.vm.objects.native_functions.get(object_id) {
                    Some(func) => write!(f, "(native-fn-{})", func.name()),
                    None => write!(f, "(native-fn-{})", object_id.as_num()),
                }
            }
            Val::BytecodeFunction(object_id) => {
                match self.vm.objects.bytecode_functions.get(object_id) {
                    Some(bc) => write!(
                        f,
                        "(fn-{})",
                        match &bc.name {
                            Some(s) => s.as_str(),
                            None => "lambda",
                        }
                    ),
                    None => write!(f, "(fn-{})", object_id.as_num()),
                }
            }
            Val::DataType(dt) => match dt {
                DataType::Void => write!(f, "(type-void)"),
                DataType::Bool => write!(f, "(type-bool)"),
                DataType::Int => write!(f, "(type-int)"),
                DataType::Float => write!(f, "(type-float)"),
                DataType::Symbol => write!(f, "(type-symbol)"),
                DataType::String => write!(f, "(type-string)"),
                DataType::List => write!(f, "(type-list)"),
                DataType::StructT => write!(f, "(type-struct)"),
                DataType::Function => write!(f, "(type-function)"),
                DataType::DataType => write!(f, "(type-type)"),
            },
        }
    }
}
