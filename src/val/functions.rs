use bumpalo::Bump;

use crate::{instruction::Instruction, vm::Vm, SporeRc};

use super::{symbol::SymbolTable, Val};

type RcNativeFunction = SporeRc<dyn Fn(&Vm) -> Val>;

#[derive(Clone)]
pub struct NativeFunction {
    f: RcNativeFunction,
}

impl<F: 'static + Fn(&Vm) -> Val> From<F> for NativeFunction {
    fn from(f: F) -> NativeFunction {
        NativeFunction { f: SporeRc::new(f) }
    }
}

impl NativeFunction {
    pub fn new<F: 'static + Fn(&[Val]) -> Val>(f: F) -> NativeFunction {
        let new_f = move |vm: &Vm| {
            let args = vm.args();
            f(args)
        };
        NativeFunction::from(new_f)
    }

    pub fn call(&self, vm: &Vm) -> Val {
        (self.f)(vm)
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
    pub fn with_str(symbols: &mut SymbolTable, arena: &Bump, s: &str) -> ByteCodeFunction {
        let instructions = crate::compiler::compile(symbols, &arena, s);
        ByteCodeFunction {
            instructions,
            args: 0,
        }
    }
}
