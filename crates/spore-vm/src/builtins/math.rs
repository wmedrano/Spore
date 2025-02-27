use crate::{
    val::{native_function::NativeFunction, Val},
    vm::{Vm, VmErrorInner, VmResult},
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_arg_list("+", plus_fn))
        .register_native_function(NativeFunction::with_arg_list("-", minus_fn))
        .register_native_function(NativeFunction::with_arg_list("<", less_fn))
        .register_native_function(NativeFunction::with_arg_list(">", greater_fn));
}

/// Adds the given arguments.
fn plus_fn(_: &Vm, args: &[Val]) -> VmResult<Val> {
    let mut int_sum = 0;
    let mut float_sum = 0.0;
    for arg in args {
        match arg {
            Val::Int(x) => int_sum += *x,
            Val::Float(x) => float_sum += *x,
            _ => return Err(VmErrorInner::WrongType)?,
        }
    }
    let res = if float_sum == 0.0 {
        Val::Int(int_sum)
    } else {
        Val::Float(float_sum + int_sum as f64)
    };
    Ok(res)
}

/// Subtracts all arguments from the first. If there is only one argument, then it is negated.
fn minus_fn(vm: &Vm, args: &[Val]) -> VmResult<Val> {
    match args {
        [] => Err(VmErrorInner::WrongArity {
            name: "-".into(),
            expected: 1,
            actual: 0,
        })?,
        [Val::Int(x)] => Ok(Val::Int(-*x)),
        [Val::Float(x)] => Ok(Val::Float(-*x)),
        [leading, rest @ ..] => {
            let sub = minus_fn(vm, &[plus_fn(vm, rest)?])?;
            Ok(plus_fn(vm, &[*leading, sub])?)
        }
    }
}

fn is_ordered_impl(args: &[Val], cmp: impl Fn(Val, Val) -> VmResult<bool>) -> VmResult<Val> {
    for window in args.windows(2) {
        match window {
            [a, b] => {
                if !cmp(*a, *b)? {
                    return Ok(Val::Bool(false));
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(Val::Bool(true))
}

/// Returns `true` if the arguments are ordered from least to greatest.
fn less_fn(_: &Vm, args: &[Val]) -> VmResult<Val> {
    is_ordered_impl(args, |a, b| match (a, b) {
        (Val::Int(a), Val::Int(b)) => Ok(a < b),
        (Val::Float(a), Val::Float(b)) => Ok(a < b),
        (Val::Int(a), Val::Float(b)) => Ok((a as f64) < b),
        (Val::Float(a), Val::Int(b)) => Ok(a < (b as f64)),
        _ => Err(VmErrorInner::WrongType)?,
    })
}

/// Returns `true` if the arguments are ordered from greatest to least.
fn greater_fn(_: &Vm, args: &[Val]) -> VmResult<Val> {
    is_ordered_impl(args, |a, b| match (a, b) {
        (Val::Int(a), Val::Int(b)) => Ok(a > b),
        (Val::Float(a), Val::Float(b)) => Ok(a > b),
        (Val::Int(a), Val::Float(b)) => Ok((a as f64) > b),
        (Val::Float(a), Val::Int(b)) => Ok(a > (b as f64)),
        _ => Err(VmErrorInner::WrongType)?,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        val::Val,
        vm::{Vm, VmErrorInner},
    };

    #[test]
    fn empty_plus_is_0() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(+)").unwrap(), Val::Int(0));
    }

    #[test]
    fn plus_with_non_number_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(+ -1 2 -3 4 false)"),
            Err(VmErrorInner::WrongType.into())
        );
    }

    #[test]
    fn plus_adds_numbers() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(+ -1 2 -3 4)").unwrap(), Val::Int(2));
        assert_eq!(
            vm.clean_eval_str("(+ -1.0 -2.0 3.0 4.0)").unwrap(),
            Val::Float(4.0)
        );
    }

    #[test]
    fn plus_with_ints_and_floats_is_float() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(+ -1 2 -3.0 4.0)").unwrap(),
            Val::Float(2.0)
        );
    }

    #[test]
    fn minus_with_no_args_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(-)"),
            Err(VmErrorInner::WrongArity {
                name: "-".into(),
                expected: 1,
                actual: 0
            }
            .into())
        );
    }

    #[test]
    fn minus_with_int_negates() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(- 1)").unwrap(), Val::Int(-1));
    }

    #[test]
    fn minus_with_float_negates() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(- 1.0)").unwrap(), Val::Float(-1.0));
    }

    #[test]
    fn minus_with_multiple_args_subtracts() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(- 1 2 3)").unwrap(), Val::Int(-4));
    }

    #[test]
    fn minus_with_multiple_floats_subtracts() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(- 1.0 2.0 3.0)").unwrap(),
            Val::Float(-4.0)
        );
    }

    #[test]
    fn minus_with_mixed_ints_and_floats_returns_float() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(- 1 2.0 3)").unwrap(), Val::Float(-4.0));
    }

    #[test]
    fn empty_less_is_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(<)").unwrap(), Val::Bool(true));
    }

    #[test]
    fn less_with_ordered_numbers_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(< -10 0.2 10)").unwrap(),
            Val::Bool(true)
        );
    }

    #[test]
    fn less_with_equal_numbers_returns_false() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(< -10 0.2 0.2 10)").unwrap(),
            Val::Bool(false)
        );
    }

    #[test]
    fn less_with_non_number_returns_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(< false true)"),
            Err(VmErrorInner::WrongType.into())
        );
    }
}
