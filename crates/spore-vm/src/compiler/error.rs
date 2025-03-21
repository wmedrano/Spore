use super::{compiler_context::CompileError, sexp::ParseError};

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents an error that can occur during compilation.
pub enum ParseOrCompileError {
    Parse(ParseError),
    Compile(CompileError),
    ModuleCompilationFoundUnexpectedLocalVariables,
}

impl From<CompileError> for ParseOrCompileError {
    fn from(value: CompileError) -> Self {
        ParseOrCompileError::Compile(value)
    }
}

impl From<ParseError> for ParseOrCompileError {
    fn from(value: ParseError) -> Self {
        ParseOrCompileError::Parse(value)
    }
}

impl std::error::Error for ParseOrCompileError {}

impl std::fmt::Display for ParseOrCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseOrCompileError::Parse(e) => write!(f, "{e}"),
            ParseOrCompileError::Compile(e) => write!(f, "{e}"),
            ParseOrCompileError::ModuleCompilationFoundUnexpectedLocalVariables => {
                write!(f, "module compilation found unexpected local variables")
            }
        }
    }
}

impl ParseOrCompileError {
    pub fn with_context(self, source: &str) -> ParseOrCompileErrorWithContext<'_> {
        ParseOrCompileErrorWithContext { err: self, source }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseOrCompileErrorWithContext<'a> {
    err: ParseOrCompileError,
    source: &'a str,
}

impl std::error::Error for ParseOrCompileErrorWithContext<'_> {}

impl std::fmt::Display for ParseOrCompileErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            ParseOrCompileError::Parse(e) => write!(f, "{}", e.with_context(self.source)),
            ParseOrCompileError::Compile(e) => write!(f, "{}", e.with_context(self.source)),
            ParseOrCompileError::ModuleCompilationFoundUnexpectedLocalVariables => {
                write!(f, "module compilation found unexpected local variables")
            }
        }
    }
}
