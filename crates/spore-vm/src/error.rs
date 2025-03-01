use compact_str::CompactString;

use crate::{
    compiler::{ast::AstError, error::CompileError},
    val::{symbol::SymbolId, DataType, Val},
    vm::Vm,
};

/// The result type for VM operations.
pub type VmResult<T> = Result<T, VmError>;

#[derive(Clone, Debug, PartialEq)]
/// Represents errors that can occur during VM execution.
pub enum VmError {
    Compile(CompileError),
    SymbolNotFound(SymbolId),
    NotCallable(Val),
    WrongType {
        expected: DataType,
        actual: DataType,
    },
    WrongArity {
        name: CompactString,
        expected: u32,
        actual: u32,
    },
    Custom(CompactString),
    Format(std::fmt::Error),
    /// Something unexpected happened with the interpreter. This is an issue with Spore itself and
    /// not the executed code.
    InterpreterBug(CompactString),
}

impl VmError {
    pub fn with_context<'a>(self, vm: &'a Vm, source: &'a str) -> VmErrorWithContext<'a> {
        VmErrorWithContext {
            vm,
            err: self,
            source,
        }
    }
}

impl std::error::Error for VmError {}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::Compile(compile_error) => write!(f, "{compile_error}"),
            VmError::SymbolNotFound(symbol_id) => {
                write!(f, "symbol {} not found", symbol_id.as_num())
            }
            VmError::NotCallable(val) => write!(f, "val {val:?} is not callable"),
            VmError::WrongType { expected, actual } => write!(
                f,
                "wrong type encountered, expected {expected:?} but got {actual:?}"
            ),
            VmError::WrongArity {
                name,
                expected,
                actual,
            } => write!(
                f,
                "wrong arity, {name} expected {expected} args, but got {actual} args."
            ),
            VmError::Custom(e) => write!(f, "custom error encountered, {e}"),
            VmError::Format(error) => write!(f, "{error}"),
            VmError::InterpreterBug(b) => write!(f, "{b}"),
        }
    }
}

impl From<CompileError> for VmError {
    fn from(value: CompileError) -> Self {
        VmError::Compile(value)
    }
}

impl From<std::fmt::Error> for VmError {
    fn from(value: std::fmt::Error) -> VmError {
        VmError::Format(value)
    }
}

impl From<AstError> for VmError {
    fn from(value: AstError) -> VmError {
        VmError::from(CompileError::from(value))
    }
}

impl<'a> From<VmErrorWithContext<'a>> for VmError {
    fn from(value: VmErrorWithContext<'a>) -> VmError {
        value.err
    }
}

pub struct VmErrorWithContext<'a> {
    vm: &'a Vm,
    err: VmError,
    source: &'a str,
}

impl std::fmt::Debug for VmErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for VmErrorWithContext<'_> {}

impl std::fmt::Display for VmErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.err {
            VmError::Compile(err) => {
                write!(f, "{}", err.with_context(self.source))
            }
            VmError::SymbolNotFound(symbol_id) => match self.vm.symbol_name(*symbol_id) {
                Some(sym) => write!(f, "symbol {sym} not found"),
                None => write!(f, "{}", self.err),
            },
            VmError::NotCallable(val) => write!(
                f,
                "Value of type {tp:?} is not callable: {val}",
                tp = val.spore_type(),
                val = val.formatted(self.vm)
            ),
            VmError::WrongType { expected, actual } => write!(
                f,
                "wrong type encountered, expected {expected:?} but got {actual:?}"
            ),
            VmError::WrongArity {
                name,
                expected,
                actual,
            } => write!(
                f,
                "wrong arity, {name} expected {expected} args, but got {actual} args."
            ),
            VmError::Custom(e) => write!(f, "custom error encountered, {e}"),
            VmError::Format(error) => write!(f, "format error: {error}"),
            VmError::InterpreterBug(b) => write!(
                f,
                "Something unexpected happened with the interpretter: {b}"
            ),
        }
    }
}
