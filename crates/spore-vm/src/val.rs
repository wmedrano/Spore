pub mod custom;
pub mod functions;
pub mod symbol;

use compact_str::CompactString;
use custom::SporeCustom;
use functions::{ByteCodeFunction, NativeFunction};
use symbol::SymbolId;

use crate::{object_store::ObjectId, vm::Vm, SporeCustomType, SporeList, SporeStruct};

/// The maximum length of a short string.
///
/// This is the highest possible value without increasing the size of `Val`. The size of `Val` is
/// checked through unit tests.
pub const SHORT_STR_LEN: usize = 14;

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct ShortString {
    len: u8,
    data: [u8; SHORT_STR_LEN],
}

impl ShortString {
    pub fn new(s: &str) -> Option<ShortString> {
        if s.len() > SHORT_STR_LEN {
            return None;
        }
        let mut data = [0; SHORT_STR_LEN];
        data[0..s.len()].copy_from_slice(s.as_bytes());
        Some(ShortString {
            len: s.len() as u8,
            data,
        })
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.data[0..self.len as usize]).unwrap()
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
/// Represents a value in the VM.
pub enum Val {
    /// Represents the lack of a value.
    #[default]
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
    String(ObjectId<CompactString>),
    /// Contains a short string.
    ShortString(ShortString),
    /// Contains a list of values.
    List(ObjectId<SporeList>),
    /// Contains a map from field to value.
    Struct(ObjectId<SporeStruct>),
    /// Contains a native function.
    NativeFunction(ObjectId<NativeFunction>),
    /// Contains a bytecode function.
    BytecodeFunction(ObjectId<ByteCodeFunction>),
    /// Contains a custom object.
    Custom(ObjectId<SporeCustom>),
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
    Custom,
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

    pub fn as_str<'a>(&'a self, vm: &'a Vm) -> Option<&'a str> {
        match self {
            Val::String(string_id) => vm.objects.get_str(*string_id),
            Val::ShortString(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn as_custom<T: SporeCustomType>(self, vm: &Vm) -> Option<&T> {
        match self {
            Val::Custom(id) => {
                let custom = vm.objects.get_custom(id)?;
                custom.get().downcast_ref()
            }
            _ => None,
        }
    }

    pub fn as_custom_mut<T: SporeCustomType>(self, vm: &mut Vm) -> Option<&mut T> {
        match self {
            Val::Custom(id) => {
                let custom = vm.objects.get_custom_mut(id)?;
                custom.get_mut().downcast_mut()
            }
            _ => None,
        }
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

    pub fn spore_type(self) -> DataType {
        match self {
            Val::Void => DataType::Void,
            Val::Bool(_) => DataType::Bool,
            Val::Int(_) => DataType::Int,
            Val::Float(_) => DataType::Float,
            Val::Symbol(_) => DataType::Symbol,
            Val::String(_) => DataType::String,
            Val::ShortString(_) => DataType::String,
            Val::List(_) => DataType::List,
            Val::Struct(_) => DataType::StructT,
            Val::NativeFunction(_) => DataType::Function,
            Val::BytecodeFunction(_) => DataType::Function,
            Val::Custom(_) => DataType::Custom,
            Val::DataType(_) => DataType::DataType,
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
            Val::ShortString(s) => write!(f, "{}", s.as_str()),
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
            Val::Custom(object_id) => match self.vm.objects.custom.get(object_id) {
                Some(obj) => write!(f, "(custom-object-{})", obj.name()),
                None => write!(f, "(gc-custom-object)"),
            },
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
                DataType::Custom => write!(f, "(type-custom)"),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn val_size_is_small() {
        assert_eq!(std::mem::size_of::<Val>(), 2 * std::mem::size_of::<usize>());
    }
}
