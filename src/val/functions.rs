use bumpalo::Bump;
use compact_str::CompactString;

use crate::{
    compiler::CompileError,
    instruction::Instruction,
    vm::{Vm, VmResult},
    SporeRc,
};

use super::{symbol::SymbolTable, Val};

type RcNativeFunction = SporeRc<dyn Fn(&mut Vm) -> VmResult<Val>>;

#[derive(Clone)]
pub struct NativeFunction {
    name: CompactString,
    f: RcNativeFunction,
}

impl NativeFunction {
    pub fn new<F: 'static + Fn(&mut Vm) -> VmResult<Val>>(name: &str, f: F) -> NativeFunction {
        NativeFunction {
            name: CompactString::new(name),
            f: SporeRc::new(f),
        }
    }

    pub fn with_args<F: 'static + Fn(&[Val]) -> VmResult<Val>>(name: &str, f: F) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            f(args)
        };
        NativeFunction::new(name, new_f)
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn call(&self, vm: &mut Vm) -> VmResult<Val> {
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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ByteCodeFunction {
    pub instructions: SporeRc<[Instruction]>,
    pub args: usize,
}

impl ByteCodeFunction {
    pub fn with_str(
        symbols: &mut SymbolTable,
        s: &str,
        arena: &Bump,
    ) -> Result<ByteCodeFunction, CompileError> {
        let instructions = crate::compiler::compile(symbols, s, arena)?;
        Ok(ByteCodeFunction {
            instructions,
            args: 0,
        })
    }
}
