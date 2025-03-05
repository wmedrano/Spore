use crate::{
    error::{VmError, VmResult},
    val::{native_function::NativeFunction, DataType, Val},
    vm::Vm,
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1("box", box_fn))
        .register_native_function(NativeFunction::with_args_1("unbox", unbox_fn))
        .register_native_function(NativeFunction::with_args_2("box-set!", box_set_fn));
}

fn box_fn(vm: &mut Vm, v: Val) -> VmResult<Val> {
    Ok(vm.make_box(v))
}

fn unbox_fn(vm: &mut Vm, v: Val) -> VmResult<Val> {
    let unboxed = v.unbox(vm).ok_or_else(|| VmError::WrongType {
        function_name: "unbox".into(),
        expected: DataType::Box,
        actual: v.spore_type(),
    })?;
    Ok(unboxed)
}

fn box_set_fn(vm: &mut Vm, boxed_val: Val, v: Val) -> VmResult<Val> {
    let b = boxed_val.unbox_mut(vm).ok_or_else(|| VmError::WrongType {
        function_name: "box-set!".into(),
        expected: DataType::Box,
        actual: v.spore_type(),
    })?;
    let ret = *b;
    *b = v;
    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boxed_value_does_not_store_literal_value_directly() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define v (box 10))").unwrap();
        assert_ne!(vm.get_global_by_name("v").unwrap(), Val::Int(10));
    }

    #[test]
    fn unboxing_box_returns_inner_value() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define v (box 10))").unwrap();
        assert_eq!(vm.clean_eval_str("(unbox v)").unwrap(), Val::Int(10));
    }

    #[test]
    fn box_set_changes_inner_value() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define v (box 10))").unwrap();
        assert_eq!(vm.clean_eval_str("(unbox v)").unwrap(), Val::Int(10));
        vm.clean_eval_str("(box-set! v 11)").unwrap();
        assert_eq!(vm.clean_eval_str("(unbox v)").unwrap(), Val::Int(11));
    }

    #[test]
    fn box_can_be_unboxed_from_rust() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define v (box \"string\"))").unwrap();
        let v = vm.get_global_by_name("v").unwrap();
        assert!(v.is_box(), "{v:?}");
        assert_eq!(v.unbox(&vm).unwrap().as_str(&vm), Some("string"));
    }
}
