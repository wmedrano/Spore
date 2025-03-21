use compact_str::CompactString;

use crate::{SporeRc, compiler::error::ParseOrCompileError, instruction::Instruction, vm::Vm};

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
    /// The number of local variables.
    pub locals: u32,
    /// The number of variables that are captured.
    pub captures: u32,
}

impl ByteCodeFunction {
    /// Creates a new bytecode function from an expression.
    ///
    /// It is assumed that the bytecode is a module definition. This implies:
    ///
    /// 1. No arguments.
    /// 2. `(define x y)` defines a global variable.
    pub fn with_module_source<'a>(
        vm: &mut Vm,
        s: &'a str,
    ) -> Result<ByteCodeFunction, ParseOrCompileError> {
        let instructions = crate::compiler::compile_module(vm, s)?;
        Ok(ByteCodeFunction {
            name: None,
            instructions,
            args: 0,
            locals: 0,
            captures: 0,
        })
    }

    /// Iterate over all values referenced by the function.
    pub fn iter_references(&self) -> impl '_ + Iterator<Item = Val> {
        self.instructions
            .iter()
            .flat_map(|instruction| match instruction {
                Instruction::Push(val) => Some(*val),
                Instruction::Deref(symbol_id) => Some(Val::Symbol(*symbol_id)),
                Instruction::Capture { id, .. } => Some(Val::BytecodeFunction {
                    id: *id,
                    captures: None,
                }),
                Instruction::Return
                | Instruction::Eval(_)
                | Instruction::Get(_)
                | Instruction::Set(_)
                | Instruction::Jump(_)
                | Instruction::JumpIf(_)
                | Instruction::Compact(_) => None,
            })
    }
}
