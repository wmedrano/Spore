/// The internal symbol used to define a new value.
pub const INTERNAL_DEFINE_FUNCTION: &str = "%define";

use crate::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError, VmResult},
};

/// Registers the built-in functions in the VM.
pub fn register_builtins(vm: &mut Vm) -> &mut Vm {
    vm.register_native_function(NativeFunction::with_args("+", plus_fn))
        .register_native_function(NativeFunction::new(INTERNAL_DEFINE_FUNCTION, define_fn))
        .register_native_function(NativeFunction::with_args("do", do_fn))
}

/// Adds the given arguments.
fn plus_fn(args: &[Val]) -> VmResult<Val> {
    let mut int_sum = 0;
    let mut float_sum = 0.0;
    for arg in args {
        match arg {
            Val::Int(x) => int_sum += *x,
            Val::Float(x) => float_sum += *x,
            _ => return Err(VmError::WrongType),
        }
    }
    let res = if float_sum == 0.0 {
        Val::Int(int_sum)
    } else {
        Val::Float(float_sum + int_sum as f64)
    };
    Ok(res)
}

/// Defines a symbol in the global scope.
fn define_fn(vm: &mut Vm) -> VmResult<Val> {
    let (sym, val) = match vm.args() {
        [Val::Symbol(sym), val] => (*sym, *val),
        _ => todo!(),
    };
    vm.globals.values.insert(sym, val);
    Ok(Val::Void)
}

/// Returns the last value in the list of arguments.
fn do_fn(args: &[Val]) -> VmResult<Val> {
    Ok(args.last().copied().unwrap_or(Val::Void))
}
