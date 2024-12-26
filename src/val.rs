use std::rc::Rc;

use bumpalo::Bump;

use crate::{compiler::compile, instruction::Instruction};

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Void,
    Int(i64),
    Float(f64),
    NativeFunction(NativeFunction),
    BytecodeFunction(ByteCodeFunction),
}

type RcNativeFunction = Rc<dyn Fn(&[Val]) -> Val>;

#[derive(Clone)]
pub struct NativeFunction(RcNativeFunction);

impl NativeFunction {
    pub fn new(f: impl 'static + Fn(&[Val]) -> Val) -> NativeFunction {
        NativeFunction(Rc::new(f))
    }

    pub fn call(&self, args: &[Val]) -> Val {
        (self.0)(args)
    }
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction").finish_non_exhaustive()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ByteCodeFunction {
    pub instructions: Vec<Instruction>,
    pub args: usize,
}

impl ByteCodeFunction {
    pub fn with_str(s: &str) -> ByteCodeFunction {
        let arena = Bump::new();
        let instructions = compile(&arena, s);
        ByteCodeFunction {
            instructions,
            args: 0,
        }
    }
}
