use error::ParseOrCompileError;
use sexp::SexpBuilder;

use crate::{SporeRc, instruction::Instruction, vm::Vm};

mod compiler_context;
pub mod error;
pub mod sexp;
pub mod span;
pub mod tokenizer;

/// Compiles a string of source code into bytecode instructions.
pub fn compile_module<'a>(
    vm: &mut Vm,
    source: &'a str,
) -> Result<SporeRc<[Instruction]>, ParseOrCompileError> {
    let mut sexp_builder = SexpBuilder::new(source);
    let mut sexps = Vec::new();
    while let Some(sexp) = sexp_builder.next(vm) {
        let sexp = sexp?;
        sexps.push(sexp);
    }
    let mut compiler = compiler_context::CompilerContext::new(vm);
    let instructions = compiler.compile_sexp_to_instructions(
        compiler_context::CompilerScope::Module,
        sexps.iter().copied(),
    )?;
    if !compiler.locals.is_empty() {
        return Err(ParseOrCompileError::ModuleCompilationFoundUnexpectedLocalVariables);
    }
    let instructions: SporeRc<[Instruction]> = instructions.into();
    Ok(instructions)
}
