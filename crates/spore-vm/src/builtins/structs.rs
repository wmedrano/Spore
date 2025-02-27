use crate::{
    error::{VmError, VmResult},
    val::{native_function::NativeFunction, Val},
    vm::Vm,
    SporeStruct,
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
                    Val::Symbol(symbol_id) => symbol_id,
                    _ => return Err(VmError::WrongType)?,
                };
                strct.insert(*key, *value);
            }
            _ => {
                return Err(VmError::WrongArity {
                    name: "struct".into(),
                    expected: args.len() as u32 + 1,
                    actual: args.len() as u32,
                })?
            }
        }
    }
    Ok(vm.make_struct(strct))
}

fn struct_get_fn(vm: &mut Vm, strct: Val, sym: Val) -> VmResult<Val> {
    let sym = match sym {
        Val::Symbol(x) => x,
        _ => return Err(VmError::WrongType)?,
    };
    let strct = strct.as_struct(vm).ok_or_else(|| VmError::WrongType)?;
    let item = strct
        .get(&sym)
        .copied()
        .ok_or_else(|| VmError::Custom("key not found".into()))?;
    Ok(item)
}

fn struct_set_fn(vm: &mut Vm, strct: Val, sym: Val, val: Val) -> VmResult<Val> {
    let sym = sym.as_symbol_id().ok_or(VmError::WrongType)?;
    let strct = strct.as_struct_mut(vm).ok_or(VmError::WrongType)?;
    strct.insert(sym, val);
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
        let val_sym1 = vm.make_symbol_id("test");
        let val_sym2 = vm.make_symbol_id("test2");
        let got = vm
            .clean_eval_str("(struct 'test 1 'test2 2)")
            .unwrap()
            .as_struct(&vm)
            .unwrap();
        assert_eq!(
            got,
            &SporeStruct::from_iter([(val_sym1, Val::Int(1)), (val_sym2, Val::Int(2))].into_iter())
        );
    }

    #[test]
    fn struct_get_returns_element_at_key() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct 'test 1 'test2 2))")
            .unwrap();
        let val_sym = vm.make_symbol("test");
        assert_eq!(
            vm.clean_eval_str("struct-get s 'test")
                .map_err(VmError::from),
            Ok(val_sym)
        );
    }

    #[test]
    fn struct_set_sets_element_at_key() {
        let mut vm = Vm::default();
        let val_sym1 = vm.make_symbol_id("test");
        let val_sym2 = vm.make_symbol_id("test2");
        vm.clean_eval_str("(define s (struct 'test 1 'test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-set! s 'test 10)")
                .map_err(VmError::from),
            Ok(Val::Void)
        );
        assert_eq!(
            vm.clean_eval_str("s").unwrap().as_struct(&vm).unwrap(),
            &SporeStruct::from_iter(
                [(val_sym1, Val::Int(10)), (val_sym2, Val::Int(2))].into_iter()
            )
        );
    }

    #[test]
    fn struct_get_returns_error_if_key_not_found() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct 'test 1 'test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-get s 'test3)")
                .map_err(VmError::from),
            Err(VmError::Custom("key not found".into()))
        );
    }

    #[test]
    fn struct_set_returns_error_if_wrong_type() {
        let mut vm = Vm::default();
        vm.clean_eval_str("(define s (struct 'test 1 'test2 2))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(struct-set! 1 'test3 3)")
                .map_err(VmError::from),
            Err(VmError::WrongType)
        );
        assert_eq!(
            vm.clean_eval_str("(struct-set! s 4 3)")
                .map_err(VmError::from),
            Err(VmError::WrongType)
        );
    }
}
