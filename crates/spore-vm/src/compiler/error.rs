use super::{ast::AstError, ir::IrError};

#[derive(Copy, Clone, Debug, PartialEq)]
/// Represents an error that can occur during compilation.
pub enum CompileError {
    Ast(AstError),
    Ir(IrError),
    ModuleCompilationFoundUnexpectedLocalVariables,
}

impl From<IrError> for CompileError {
    fn from(value: IrError) -> Self {
        CompileError::Ir(value)
    }
}

impl From<AstError> for CompileError {
    fn from(value: AstError) -> Self {
        CompileError::Ast(value)
    }
}

impl std::error::Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Ast(e) => write!(f, "{e}"),
            CompileError::Ir(e) => write!(f, "{e}"),
            CompileError::ModuleCompilationFoundUnexpectedLocalVariables => {
                write!(f, "module compilation found unexpected local variables")
            }
        }
    }
}

impl CompileError {
    pub fn with_context(self, source: &str) -> CompileErrorWithContext<'_> {
        CompileErrorWithContext { err: self, source }
    }
}

#[derive(Debug, PartialEq)]
pub struct CompileErrorWithContext<'a> {
    err: CompileError,
    source: &'a str,
}

impl std::error::Error for CompileErrorWithContext<'_> {}

impl std::fmt::Display for CompileErrorWithContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.err {
            CompileError::Ast(e) => write!(f, "{}", e.with_context(self.source)),
            CompileError::Ir(e) => write!(f, "{}", e.with_context(self.source)),
            CompileError::ModuleCompilationFoundUnexpectedLocalVariables => {
                write!(f, "module compilation found unexpected local variables")
            }
        }
    }
}
