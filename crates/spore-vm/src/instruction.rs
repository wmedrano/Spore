use crate::{
    val::{symbol::SymbolId, Val},
    vm::Vm,
};

#[derive(Clone, Debug, PartialEq)]
/// Represents a single instruction in the bytecode.
pub enum Instruction {
    /// Returns from a function.
    Return,
    /// Pushes a value onto the stack.
    Push(Val),
    /// Evaluates a function call.
    Eval(usize),
    /// Gets a value from the stack.
    Get(usize),
    /// Dereferences a symbol.
    Deref(SymbolId),
    /// Jump N instructions.
    Jump(usize),
    /// Jump N instructions if the top value in the stack is truthy.
    JumpIf(usize),
    /// Compact the last N elements, leaving only the top one.
    Compact(usize),
}

pub struct InstructionFormatter<'a> {
    vm: &'a Vm,
    instruction: &'a Instruction,
}

impl Instruction {
    pub fn formatted<'a>(&'a self, vm: &'a Vm) -> InstructionFormatter<'a> {
        InstructionFormatter {
            vm,
            instruction: self,
        }
    }
}

impl std::fmt::Display for InstructionFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.instruction {
            Instruction::Return => write!(f, "Return"),
            Instruction::Push(val) => write!(f, "Push({})", val.formatted(self.vm)),
            Instruction::Eval(n) => write!(f, "Eval({n})"),
            Instruction::Get(n) => write!(f, "Get({n})"),
            Instruction::Deref(symbol_id) => {
                write!(f, "Deref({})", Val::Symbol(*symbol_id).formatted(self.vm))
            }
            Instruction::Jump(n) => write!(f, "Jump({n})"),
            Instruction::JumpIf(n) => write!(f, "JumpIf({n})"),
            Instruction::Compact(n) => write!(f, "Compact({n})"),
        }
    }
}
