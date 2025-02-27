use crate::{
    val::{native_function::NativeFunction, Val},
    vm::{Vm, VmError, VmResult},
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::new("list", list_fn))
        .register_native_function(NativeFunction::with_args_2("list-concat", list_concat_fn))
        .register_native_function(NativeFunction::with_args_2("doall", doall_fn))
        .register_native_function(NativeFunction::with_args_2("map", map_fn))
        .register_native_function(NativeFunction::with_args_2("filter", filter_fn))
        .register_native_function(NativeFunction::with_args_1("list-len", list_len_fn))
        .register_native_function(NativeFunction::with_args_1("list-empty?", list_empty_fn))
        .register_native_function(NativeFunction::with_args_2("nth", nth_fn));
}

fn list_fn(vm: &mut Vm) -> VmResult<Val> {
    let args = vm.args().to_vec();
    Ok(vm.make_list(args))
}

fn list_concat_fn(vm: &mut Vm, a: Val, b: Val) -> VmResult<Val> {
    let a = a.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    let b = b.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    let c = Vec::from_iter(a.iter().chain(b.iter()).copied());
    Ok(vm.make_list(c))
}

fn doall_fn(vm: &mut Vm, f: Val, lst: Val) -> VmResult<Val> {
    let len = list_len_fn(vm, lst)?.as_int().unwrap();
    for idx in 0..len {
        let element = nth_fn(vm, lst, Val::Int(idx))?;
        vm.eval_function(f, &[element])?;
    }
    Ok(Val::Void)
}

fn map_fn(vm: &mut Vm, f: Val, lst: Val) -> VmResult<Val> {
    let len = list_len_fn(vm, lst)?.as_int().unwrap();
    let new_list_start = vm.stack.len();
    for idx in 0..len {
        let element = nth_fn(vm, lst, Val::Int(idx))?;
        let res = vm.eval_function(f, &[element])?;
        // TODO: Store this somewhere else instead of polluting the stack.
        vm.stack.push(res);
    }
    let ret = Vec::from_iter(vm.stack.drain(new_list_start..));
    Ok(vm.make_list(ret))
}

fn filter_fn(vm: &mut Vm, pred: Val, lst: Val) -> VmResult<Val> {
    let len = list_len_fn(vm, lst)?.as_int().unwrap();
    let new_list_start = vm.stack.len();
    for idx in 0..len {
        let element = nth_fn(vm, lst, Val::Int(idx))?;
        let should_keep = vm.eval_function(pred, &[element])?.is_truthy();
        if should_keep {
            // TODO: Store this somewhere else instead of polluting the stack.
            vm.stack.push(element);
        }
    }
    let ret = Vec::from_iter(vm.stack.drain(new_list_start..));
    Ok(vm.make_list(ret))
}

fn list_len_fn(vm: &mut Vm, lst: Val) -> VmResult<Val> {
    let lst = lst.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    Ok(Val::Int(lst.len() as i64))
}

fn list_empty_fn(vm: &mut Vm, lst: Val) -> VmResult<Val> {
    let lst = lst.as_list(vm).ok_or_else(|| VmError::WrongType)?;
    Ok(Val::Bool(lst.is_empty()))
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
        let got = vm.clean_eval_str("(list)").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[]);
    }

    #[test]
    fn list_with_one_arg_returns_list_with_one_element() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(list 1)").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[Val::Int(1)]);
    }

    #[test]
    fn list_with_multiple_args_returns_list_with_all_elements() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(list 1 2 3)").unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(1), Val::Int(2), Val::Int(3)]
        );
    }

    #[test]
    fn list_with_different_types_of_args() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(list 1 2.0 true)").unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(1), Val::Float(2.0), Val::Bool(true)]
        );
    }

    #[test]
    fn map_on_empty_list_returns_empty_list() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define (double x) (+ x x))").unwrap();
        let got = vm.clean_eval_str("(map double (list))").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[]);
    }

    #[test]
    fn map_applies_fn_to_all_elements() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define (double x) (+ x x))").unwrap();
        let got = vm.clean_eval_str("(map double (list 1 2 3))").unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(2), Val::Int(4), Val::Int(6)]
        );
    }

    #[test]
    fn filter_on_empty_list_returns_empty_list() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define (zero? x) (= x 0))").unwrap();
        let got = vm.clean_eval_str("(filter zero? (list))").unwrap();
        assert_eq!(got.as_list(&vm).unwrap(), &[]);
    }

    #[test]
    fn filter_keeps_only_elements_that_pass_predicate() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define (zero? x) (= 0 x))").unwrap();
        let got = vm
            .clean_eval_str("(filter zero? (list 0 1 0 2 0 3))")
            .unwrap();
        assert_eq!(
            got.as_list(&vm).unwrap(),
            &[Val::Int(0), Val::Int(0), Val::Int(0)]
        );
    }

    #[test]
    fn list_len_returns_the_length_of_the_list() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define lst (list 1 2 3))").unwrap();
        let got = vm.clean_eval_str("(list-len lst)").unwrap();
        assert_eq!(got, Val::Int(3));
    }

    #[test]
    fn list_len_returns_0_for_empty_list() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define lst (list))").unwrap();
        let got = vm.clean_eval_str("(list-len lst)").unwrap();
        assert_eq!(got, Val::Int(0));
    }

    #[test]
    fn list_len_errors_if_not_a_list() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(list-len 1)");
        assert_eq!(got, Err(VmError::WrongType));
    }

    #[test]
    fn nth_returns_element_at_index() {
        let mut vm = Vm::default();
        let got = vm
            .clean_eval_str("(define lst (list 1 2 3)) (nth lst 1)")
            .unwrap();
        assert_eq!(got, Val::Int(2));
    }

    #[test]
    fn nth_returns_error_if_index_out_of_bounds() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(define lst (list 1 2 3)) (nth lst 3)");
        assert_eq!(got, Err(VmError::Custom("index out of range".into())));
    }

    #[test]
    fn nth_returns_error_if_index_is_negative() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(define lst (list 1 2 3)) (nth lst -1)");
        assert_eq!(got, Err(VmError::Custom("negative idx provided".into())));
    }

    #[test]
    fn nth_returns_error_if_first_arg_is_not_list() {
        let mut vm = Vm::default();
        let got = vm.clean_eval_str("(nth 1 1)");
        assert_eq!(got, Err(VmError::WrongType));
    }

    #[test]
    fn nth_returns_error_if_second_arg_is_not_int() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define lst (list 1 2 3))").unwrap();
        let got = vm.clean_eval_str("(nth lst true)");
        assert_eq!(got, Err(VmError::WrongType));
    }
}
