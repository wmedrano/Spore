mod math;

/// The internal symbol used to define a new value.
pub const INTERNAL_DEFINE_FUNCTION: &str = "%define";

use crate::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError, VmResult},
};

/// Registers the built-in functions in the VM.
pub fn register_builtins(vm: &mut Vm) -> &mut Vm {
    vm.register_native_function(NativeFunction::new(INTERNAL_DEFINE_FUNCTION, define_fn))
        .register_native_function(NativeFunction::with_args("do", do_fn))
        .register_native_function(NativeFunction::with_args("throw", throw_fn));
    math::register(vm)
}

/// Defines a symbol in the global scope.
fn define_fn(vm: &mut Vm) -> VmResult<Val> {
    let global_id = vm.common_symbols.global;
    let (sym, val) = match vm.args() {
        [Val::Symbol(sym), val] => (*sym, *val),
        _ => todo!(),
    };
    vm.modules
        .get_mut(&global_id)
        .expect("%global module not found")
        .values
        .insert(sym, val);
    Ok(Val::Void)
}

/// Returns the last value in the list of arguments.
fn do_fn(args: &[Val]) -> VmResult<Val> {
    Ok(args.last().copied().unwrap_or(Val::Void))
}

/// Throw an error.
fn throw_fn(_: &[Val]) -> VmResult<Val> {
    Err(VmError::Custom("exception thrown".into()))
}

#[cfg(test)]
mod tests {
    use crate::{val::Val, vm::Vm, vm::VmError};

    #[test]
    fn define_binds_values_to_name() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(%define 'x 12)"), Ok(Val::Void));
        assert_eq!(vm.eval_str("x"), Ok(Val::Int(12)));
    }

    #[test]
    fn do_returns_last_value_or_void_if_empty() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(do)"), Ok(Val::Void));
        assert_eq!(vm.eval_str("(do 1)"), Ok(Val::Int(1)));
        assert_eq!(vm.eval_str("(do 1 2)"), Ok(Val::Int(2)));
    }

    #[test]
    fn throw_returns_an_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(throw)"),
            Err(VmError::Custom("exception thrown".into()))
        );
        assert_eq!(
            vm.eval_str("(throw 1)"),
            Err(VmError::Custom("exception thrown".into()))
        );
        assert_eq!(
            vm.eval_str("(throw 1 2 3)"),
            Err(VmError::Custom("exception thrown".into()))
        );
    }
}
