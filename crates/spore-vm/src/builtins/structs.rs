use crate::{
    SporeStruct,
    error::{VmError, VmResult},
    val::{DataType, Val, native_function::NativeFunction},
    vm::Vm,
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::new("struct", struct_fn))
        .register_native_function(NativeFunction::with_args_2("struct-get", struct_get_fn))
        .register_native_function(NativeFunction::with_args_3("struct-set!", struct_set_fn));
}

fn struct_fn(vm: &mut Vm) -> VmResult<Val> {
    let args = vm.args();
    let mut strct = SporeStruct::with_capacity(args.len() / 2);
    for pair in args.chunks(2) {
        match pair {
            [key, value] => {
                let key = match key {
                    Val::Key(key_id) => key_id,
                    _ => {
                        return Err(VmError::WrongType {
                            function_name: "struct".into(),
                            expected: DataType::Key,
                            actual: key.spore_type(),
                        })?;
                    }
                };
                strct.insert(*key, *value);
            }
            _ => {
                return Err(VmError::WrongArity {
                    function_name: "struct".into(),
                    expected: args.len() as u32 + 1,
                    actual: args.len() as u32,
                })?;
            }
        }
    }
    Ok(vm.make_struct(strct))
}

fn struct_get_fn(vm: &mut Vm, strct: Val, key: Val) -> VmResult<Val> {
    let k = match key {
        Val::Key(x) => x,
        _ => {
            return Err(VmError::WrongType {
                function_name: "struct-get".into(),
                expected: DataType::Key,
                actual: key.spore_type(),
            })?;
        }
    };
    let strct = strct.as_struct(vm).ok_or_else(|| VmError::WrongType {
        function_name: "struct-get".into(),
        expected: DataType::StructT,
        actual: strct.spore_type(),
    })?;
    let item = strct
        .get(&k)
        .copied()
        .ok_or_else(|| VmError::Custom("key not found".into()))?;
    Ok(item)
}

fn struct_set_fn(vm: &mut Vm, strct: Val, key: Val, val: Val) -> VmResult<Val> {
    let k = key.as_key_id().ok_or(VmError::WrongType {
        function_name: "".into(),
        expected: DataType::Key,
        actual: key.spore_type(),
    })?;
    let strct = strct.as_struct_mut(vm).ok_or(VmError::WrongType {
        function_name: "".into(),
        expected: DataType::StructT,
        actual: strct.spore_type(),
    })?;
    strct.insert(k, val);
    Ok(Val::Void)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Vm;

    #[test]
    fn struct_with_no_args_returns_empty_struct() {
        let mut vm = Vm::default();
        let got = vm
            .clean_eval_str("(struct)")
            .unwrap()
            .as_struct(&vm)
            .unwrap();
        assert_eq!(got, &SporeStruct::new());
    }

    #[test]
    fn struct_with_args_returns_struct_with_elements() {
        let mut vm = Vm::default();
        let val_k1 = vm.make_identifier_id("k1");
        let val_k2 = vm.make_identifier_id("k2");
        let got = vm
            .clean_eval_str("(struct :k1 1 :k2 2)")
            .unwrap()
            .as_struct(&vm)
            .unwrap();
        assert_eq!(
            got,
            &SporeStruct::from_iter([(val_k1, Val::Int(1)), (val_k2, Val::Int(2))].into_iter())
        );
    }

    #[test]
    fn struct_get_returns_element_at_key() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct :test 1 :test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-get s :test)")
                .map_err(VmError::from),
            Ok(Val::Int(1))
        );
    }

    #[test]
    fn struct_set_sets_element_at_key() {
        let mut vm = Vm::default();
        let val_k1 = vm.make_identifier_id("k1");
        let val_k2 = vm.make_identifier_id("k2");
        vm.clean_eval_str("(define s (struct :k1 1 :k2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-set! s :k1 10)")
                .map_err(VmError::from),
            Ok(Val::Void)
        );
        assert_eq!(
            vm.clean_eval_str("s").unwrap().as_struct(&vm).unwrap(),
            &SporeStruct::from_iter([(val_k1, Val::Int(10)), (val_k2, Val::Int(2))].into_iter())
        );
    }

    #[test]
    fn struct_get_returns_error_if_key_not_found() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct :test 1 :test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-get s :test3)")
                .map_err(VmError::from),
            Err(VmError::Custom("key not found".into()))
        );
    }

    #[test]
    fn struct_set_returns_error_if_wrong_type() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct :test 1 :test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-set! 1 :test3 3)")
                .map_err(VmError::from),
            Err(VmError::WrongType {
                function_name: "".into(),
                expected: DataType::StructT,
                actual: DataType::Int,
            })
        );
        assert_eq!(
            vm.clean_eval_str("(struct-set! s 4 3)")
                .map_err(VmError::from),
            Err(VmError::WrongType {
                function_name: "".into(),
                expected: DataType::Key,
                actual: DataType::Int,
            })
        );
    }
}
