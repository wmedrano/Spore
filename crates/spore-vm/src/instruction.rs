use crate::{
    gc::ObjectId,
    val::{Val, bytecode_function::ByteCodeFunction, identifier::IdentifierId},
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
    /// Sets the value for the stakc.
    Set(usize),
    /// Dereferences a symbol.
    Deref(IdentifierId),
    /// Jump N instructions.
    Jump(usize),
    /// Jump N instructions if the top value in the stack is truthy.
    JumpIf(usize),
    /// Compact the last N elements, leaving only the top one.
    Compact(usize),
    /// Combine the top `capture_count` values of the stack with `id` to push a new `Val` containing
    /// the lambda with its captured environment.
    Capture {
        id: ObjectId<ByteCodeFunction>,
        capture_count: u32,
    },
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
            Instruction::Set(n) => write!(f, "Set({n})"),
            Instruction::Deref(symbol_id) => {
                write!(f, "Deref({})", Val::Symbol(*symbol_id).formatted(self.vm))
            }
            Instruction::Jump(n) => write!(f, "Jump({n})"),
            Instruction::JumpIf(n) => write!(f, "JumpIf({n})"),
            Instruction::Compact(n) => write!(f, "Compact({n})"),
            Instruction::Capture { capture_count, id } => {
                write!(f, "Capture({id:?}, {capture_count})")
            }
        }
    }
}
