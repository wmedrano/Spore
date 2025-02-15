use crate::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError, VmResult},
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::new("list", list_fn))
        .register_native_function(NativeFunction::with_args_1("list-len", list_len_fn))
        .register_native_function(NativeFunction::with_args_2("nth", nth_fn));
}

fn list_fn(vm: &mut Vm) -> VmResult<Val> {
    let args = vm.args().to_vec();
    let lst = vm.objects.register_list(args);
    Ok(Val::List(lst))
}

fn list_len_fn(vm: &mut Vm, lst: Val) -> VmResult<Val> {
    let lst = lst.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    Ok(Val::Int(lst.len() as i64))
}

fn nth_fn(vm: &mut Vm, lst: Val, idx: Val) -> VmResult<Val> {
    let idx = match idx {
        Val::Int(x) => {
            if x < 0 {
                return Err(VmError::Custom("negative idx provided".into()));
            } else {
                x as usize
            }
        }
        _ => return Err(VmError::WrongType),
    };
    let lst = lst.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    lst.get(idx)
        .copied()
        .ok_or_else(|| VmError::Custom("index out of range".into()))
}

#[cfg(test)]
mod tests {
    use crate::{
        val::Val,
        vm::{Vm, VmError},
    };

    #[test]
    fn list_with_no_args_returns_empty_list() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(list)").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[]);
    }

    #[test]
    fn list_with_one_arg_returns_list_with_one_element() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(list 1)").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[Val::Int(1)]);
    }

    #[test]
    fn list_with_multiple_args_returns_list_with_all_elements() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(list 1 2 3)").unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(1), Val::Int(2), Val::Int(3)]
        );
    }

    #[test]
    fn list_with_different_types_of_args() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(list 1 2.0 true)").unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(1), Val::Float(2.0), Val::Bool(true)]
        );
    }

    #[test]
    fn list_len_returns_the_length_of_the_list() {
        let mut vm = Vm::default();
        vm.eval_str("(define lst (list 1 2 3))").unwrap();
        let got = vm.eval_str("(list-len lst)").unwrap();
        assert_eq!(got, Val::Int(3));
    }

    #[test]
    fn list_len_returns_0_for_empty_list() {
        let mut vm = Vm::default();
        vm.eval_str("(define lst (list))").unwrap();
        let got = vm.eval_str("(list-len lst)").unwrap();
        assert_eq!(got, Val::Int(0));
    }

    #[test]
    fn list_len_errors_if_not_a_list() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(list-len 1)");
        assert_eq!(got, Err(VmError::WrongType));
    }

    #[test]
    fn nth_returns_element_at_index() {
        let mut vm = Vm::default();
        let got = vm
            .eval_str("(do (define lst (list 1 2 3)) (nth lst 1))")
            .unwrap();
        assert_eq!(got, Val::Int(2));
    }

    #[test]
    fn nth_returns_error_if_index_out_of_bounds() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(do (define lst (list 1 2 3)) (nth lst 3))");
        assert_eq!(got, Err(VmError::Custom("index out of range".into())));
    }

    #[test]
    fn nth_returns_error_if_index_is_negative() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(do (define lst (list 1 2 3)) (nth lst -1))");
        assert_eq!(got, Err(VmError::Custom("negative idx provided".into())));
    }

    #[test]
    fn nth_returns_error_if_first_arg_is_not_list() {
        let mut vm = Vm::default();
        let got = vm.eval_str("(nth 1 1)");
        assert_eq!(got, Err(VmError::WrongType));
    }

    #[test]
    fn nth_returns_error_if_second_arg_is_not_int() {
        let mut vm = Vm::default();
        vm.eval_str("(define lst (list 1 2 3))").unwrap();
        let got = vm.eval_str("(nth lst true)");
        assert_eq!(got, Err(VmError::WrongType));
    }
}
