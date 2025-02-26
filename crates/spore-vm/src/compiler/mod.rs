use std::collections::HashSet;

use ast::Ast;
use bumpalo::Bump;
use error::CompileError;

use crate::{instruction::Instruction, vm::Vm, SporeRc};

pub mod ast;
mod compiler_context;
pub mod error;
mod ir;
mod parsed_text;
pub mod span;
pub mod tokenizer;

/// Compiles a string of source code into bytecode instructions.
pub fn compile_module<'a>(
    vm: &mut Vm,
    source: &'a str,
    asts: impl Iterator<Item = &'a Ast>,
    arena: &'a Bump,
) -> Result<SporeRc<[Instruction]>, CompileError> {
    let mut instructions = Vec::new();
    let mut compiler = compiler_context::CompilerContext {
        vm,
        args: &[],
        locals: Vec::new(),
        capturable: HashSet::new(),
        captures: Vec::new(),
    };
    let ir = ir::Ir::with_ast(source, asts, arena)?;
    compiler.compile(
        &mut instructions,
        compiler_context::CompilerScope::Module,
        &ir,
    )?;
    if !compiler.locals.is_empty() {
        return Err(CompileError::ModuleCompilationFoundUnexpectedLocalVariables);
    }
    let instructions: SporeRc<[Instruction]> = instructions.into();
    Ok(instructions)
}
