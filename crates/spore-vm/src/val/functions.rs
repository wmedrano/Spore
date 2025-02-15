use bumpalo::Bump;
use compact_str::CompactString;

use crate::{
    compiler::{ast::Ast, CompileError},
    instruction::Instruction,
    vm::{Vm, VmError, VmResult},
    SporeRc,
};

use super::Val;

type RcNativeFunction = SporeRc<dyn Fn(&mut Vm) -> VmResult<Val>>;

#[derive(Clone)]
/// Represents a native (Rust) function.
pub struct NativeFunction {
    name: CompactString,
    f: RcNativeFunction,
}

impl NativeFunction {
    /// Creates a new native function that takes no arguments.
    pub fn new<F: 'static + Fn(&mut Vm) -> VmResult<Val>>(name: &str, f: F) -> NativeFunction {
        NativeFunction {
            name: CompactString::new(name),
            f: SporeRc::new(f),
        }
    }

    /// Creates a new native function that takes arguments.
    pub fn with_arg_list<F: 'static + Fn(&Vm, &[Val]) -> VmResult<Val>>(
        name: &str,
        f: F,
    ) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            f(vm, args)
        };
        NativeFunction::new(name, new_f)
    }

    /// Creates a new native function that takes no args.
    pub fn with_args_0<F: 'static + Fn(&mut Vm) -> VmResult<Val>>(
        name: &str,
        f: F,
    ) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            match args {
                [] => f(vm),
                _ => Err(VmError::WrongArity {
                    expected: 0,
                    actual: args.len() as u32,
                }),
            }
        };
        NativeFunction::new(name, new_f)
    }

    /// Creates a new native function that takes a single argument.
    pub fn with_args_1<F: 'static + Fn(&mut Vm, Val) -> VmResult<Val>>(
        name: &str,
        f: F,
    ) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            match args {
                [arg] => f(vm, *arg),
                _ => Err(VmError::WrongArity {
                    expected: 1,
                    actual: args.len() as u32,
                }),
            }
        };
        NativeFunction::new(name, new_f)
    }

    /// Creates a new native function that takes two arguments.
    pub fn with_args_2<F: 'static + Fn(&mut Vm, Val, Val) -> VmResult<Val>>(
        name: &str,
        f: F,
    ) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            match args {
                [arg1, arg2] => f(vm, *arg1, *arg2),
                _ => Err(VmError::WrongArity {
                    expected: 2,
                    actual: args.len() as u32,
                }),
            }
        };
        NativeFunction::new(name, new_f)
    }

    /// Creates a new native function that takes three arguments.
    pub fn with_arg_3<F: 'static + Fn(&mut Vm, Val, Val, Val) -> VmResult<Val>>(
        name: &str,
        f: F,
    ) -> NativeFunction {
        let new_f = move |vm: &mut Vm| {
            let args = vm.args();
            match args {
                [arg1, arg2, arg3] => f(vm, *arg1, *arg2, *arg3),
                _ => Err(VmError::WrongArity {
                    expected: 3,
                    actual: args.len() as u32,
                }),
            }
        };
        NativeFunction::new(name, new_f)
    }

    /// Returns the name of the function.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Calls the function.
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
    pub fn new(
        vm: &mut Vm,
        s: &str,
        ast: &Ast,
        arena: &Bump,
    ) -> Result<ByteCodeFunction, CompileError> {
        let instructions = crate::compiler::compile(vm, s, std::iter::once(ast), arena)?;
        Ok(ByteCodeFunction {
            name: None,
            instructions,
            args: 0,
        })
    }
}
