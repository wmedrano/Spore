use bumpalo::Bump;
use compact_str::CompactString;

use crate::{
    compiler::{ast::Ast, CompileError},
    instruction::Instruction,
    vm::Vm,
    SporeRc,
};

use super::Val;

#[derive(Clone, Debug, Default, PartialEq)]
/// Represents a bytecode function.
pub struct ByteCodeFunction {
    /// The name of the function.
    pub name: Option<CompactString>,
    /// The instructions of the function.
    pub instructions: SporeRc<[Instruction]>,
    /// The number of arguments the function takes.
    pub args: u32,
}

impl ByteCodeFunction {
    /// Creates a new bytecode function from an Ast.
    pub fn new<'a>(
        vm: &mut Vm,
        s: &'a str,
        ast: impl Iterator<Item = &'a Ast>,
        arena: &'a Bump,
    ) -> Result<ByteCodeFunction, CompileError> {
        let instructions = crate::compiler::compile_module(vm, s, ast, arena)?;
        Ok(ByteCodeFunction {
            name: None,
            instructions,
            args: 0,
        })
    }

    /// Iterate over all values referenced by the function.
    pub fn iter_references(&self) -> impl '_ + Iterator<Item = Val> {
        self.instructions
            .iter()
            .flat_map(|instruction| match instruction {
                Instruction::Push(val) => Some(*val),
                Instruction::Deref(symbol_id) => Some(Val::Symbol(*symbol_id)),
                Instruction::Return
                | Instruction::Eval(_)
                | Instruction::Get(_)
                | Instruction::Jump(_)
                | Instruction::JumpIf(_)
                | Instruction::Compact(_) => None,
            })
    }
}
