use bumpalo::Bump;

use crate::{compiler::compile, instruction::Instruction, object_store::ObjectId, SporeRc};

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Void,
    Int(i64),
    Float(f64),
    NativeFunction(ObjectId<NativeFunction>),
    BytecodeFunction(ObjectId<ByteCodeFunction>),
}

type RcNativeFunction = SporeRc<dyn Fn(&[Val]) -> Val>;

#[derive(Clone)]
pub struct NativeFunction {
    f: RcNativeFunction,
}

impl NativeFunction {
    pub fn new(f: impl 'static + Fn(&[Val]) -> Val) -> NativeFunction {
        NativeFunction { f: SporeRc::new(f) }
    }

    pub fn call(&self, args: &[Val]) -> Val {
        (self.f)(args)
    }
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction").finish_non_exhaustive()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(SporeRc::as_ptr(&self.f), SporeRc::as_ptr(&other.f))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ByteCodeFunction {
    pub instructions: SporeRc<[Instruction]>,
    pub args: usize,
}

impl ByteCodeFunction {
    pub fn with_str(arena: &Bump, s: &str) -> ByteCodeFunction {
        let instructions = compile(&arena, s);
        ByteCodeFunction {
            instructions,
            args: 0,
        }
    }
}
