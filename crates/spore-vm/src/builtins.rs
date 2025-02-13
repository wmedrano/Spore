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
        .register_native_function(NativeFunction::with_args("throw", throw_fn))
        .register_native_function(NativeFunction::with_args("+", plus_fn))
        .register_native_function(NativeFunction::with_args("-", minus_fn))
        .register_native_function(NativeFunction::with_args("<", less_fn))
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

fn minus_fn(args: &[Val]) -> VmResult<Val> {
    match args {
        [] => Err(VmError::WrongArity {
            expected: 1,
            actual: 0,
        }),
        [Val::Int(x)] => Ok(Val::Int(-*x)),
        [Val::Float(x)] => Ok(Val::Float(-*x)),
        [leading, rest @ ..] => {
            let sub = minus_fn(&[plus_fn(rest)?])?;
            Ok(plus_fn(&[*leading, sub])?)
        }
    }
}

/// Returns `true` if the arguments are ordered from least to greatest.
fn less_fn(args: &[Val]) -> VmResult<Val> {
    for window in args.windows(2) {
        match window {
            [a, b] => {
                let is_less = match (a, b) {
                    (Val::Int(a), Val::Int(b)) => a < b,
                    (Val::Float(a), Val::Float(b)) => a < b,
                    (Val::Int(a), Val::Float(b)) => (*a as f64) < *b,
                    (Val::Float(a), Val::Int(b)) => *a < (*b as f64),
                    _ => return Err(VmError::WrongType),
                };
                if !is_less {
                    return Ok(Val::Bool(false));
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(Val::Bool(true))
}

#[cfg(test)]
mod tests {
    use crate::{
        val::Val,
        vm::{Vm, VmError},
    };

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
    fn empty_plus_is_0() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(+)").unwrap(), Val::Int(0));
    }

    #[test]
    fn plus_with_non_number_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(+ -1 2 -3 4 false)"), Err(VmError::WrongType));
    }

    #[test]
    fn plus_adds_numbers() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(+ -1 2 -3 4)").unwrap(), Val::Int(2));
        assert_eq!(
            vm.eval_str("(+ -1.0 -2.0 3.0 4.0)").unwrap(),
            Val::Float(4.0)
        );
    }

    #[test]
    fn plus_with_ints_and_floats_is_float() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(+ -1 2 -3.0 4.0)").unwrap(), Val::Float(2.0));
    }

    #[test]
    fn empty_less_is_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(<)").unwrap(), Val::Bool(true));
    }

    #[test]
    fn less_with_ordered_numbers_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(< -10 0.2 10)").unwrap(), Val::Bool(true));
    }

    #[test]
    fn less_with_equal_numbers_returns_false() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(< -10 0.2 0.2 10)").unwrap(), Val::Bool(false));
    }

    #[test]
    fn less_with_non_number_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(< false true)"), Err(VmError::WrongType));
    }

    #[test]
    fn minus_with_no_args_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.eval_str("(-)"),
            Err(VmError::WrongArity {
                expected: 1,
                actual: 0
            })
        );
    }

    #[test]
    fn minus_with_int_negates() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(- 1)").unwrap(), Val::Int(-1));
    }

    #[test]
    fn minus_with_float_negates() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(- 1.0)").unwrap(), Val::Float(-1.0));
    }

    #[test]
    fn minus_with_multiple_args_subtracts() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(- 1 2 3)").unwrap(), Val::Int(-4));
    }

    #[test]
    fn minus_with_multiple_floats_subtracts() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(- 1.0 2.0 3.0)").unwrap(), Val::Float(-4.0));
    }

    #[test]
    fn minus_with_mixed_ints_and_floats_returns_float() {
        let mut vm = Vm::default();
        assert_eq!(vm.eval_str("(- 1 2.0 3)").unwrap(), Val::Float(-4.0));
    }
}
