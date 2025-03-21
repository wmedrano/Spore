use compact_str::CompactString;

use crate::{
    compiler::{error::ParseOrCompileError, sexp::ParseError},
    val::{DataType, Val, identifier::IdentifierId},
    vm::Vm,
};

/// The result type for VM operations.
pub type VmResult<T> = Result<T, VmError>;

#[derive(Clone, Debug, PartialEq)]
/// Represents errors that can occur during VM execution.
pub enum VmError {
    Compile(ParseOrCompileError),
    IdentifierNotFound(IdentifierId),
    NotCallable(Val),
    WrongType {
        function_name: CompactString,
        expected: DataType,
        actual: DataType,
    },
    WrongArity {
        function_name: CompactString,
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
            VmError::IdentifierNotFound(symbol_id) => {
                write!(f, "symbol {} not found", symbol_id.as_num())
            }
            VmError::NotCallable(val) => write!(f, "val {val:?} is not callable"),
            VmError::WrongType {
                function_name: name,
                expected,
                actual,
            } => write!(
                f,
                "wrong type encountered in {name}, expected {expected:?} but got {actual:?}"
            ),
            VmError::WrongArity {
                function_name: name,
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

impl From<ParseOrCompileError> for VmError {
    fn from(value: ParseOrCompileError) -> Self {
        VmError::Compile(value)
    }
}

impl From<std::fmt::Error> for VmError {
    fn from(value: std::fmt::Error) -> VmError {
        VmError::Format(value)
    }
}

impl From<ParseError> for VmError {
    fn from(value: ParseError) -> VmError {
        VmError::from(ParseOrCompileError::from(value))
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
            VmError::IdentifierNotFound(symbol_id) => match self.vm.identifier_name(*symbol_id) {
                Some(sym) => write!(f, "symbol {sym} not found"),
                None => write!(f, "{}", self.err),
            },
            VmError::NotCallable(val) => write!(
                f,
                "Value of type {tp:?} is not callable: {val}",
                tp = val.spore_type(),
                val = val.formatted(self.vm)
            ),
            VmError::WrongType {
                function_name: name,
                expected,
                actual,
            } => write!(
                f,
                "wrong type encountered in {name}, expected {expected:?} but got {actual:?}"
            ),
            VmError::WrongArity {
                function_name: name,
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
