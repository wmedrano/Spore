mod lists;
mod math;
mod strings;
mod structs;

/// The internal symbol used to define a new value.
pub const INTERNAL_DEFINE_FUNCTION: &str = "%define";

use compact_str::format_compact;

use crate::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError, VmResult},
    SporeList,
};

/// Registers the built-in functions in the VM.
pub fn register_builtins(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_2(
        INTERNAL_DEFINE_FUNCTION,
        define_fn,
    ))
    .register_native_function(NativeFunction::with_args_2("apply", apply_fn))
    .register_native_function(NativeFunction::with_arg_list("do", do_fn))
    .register_native_function(NativeFunction::with_arg_list("throw", throw_fn))
    .register_native_function(NativeFunction::with_args_1("val->string", val_to_string_fn))
    .register_native_function(NativeFunction::with_args_1("val->type", val_to_type_fn))
    .register_native_function(NativeFunction::with_args_2("=", equal_fn))
    .register_native_function(NativeFunction::with_args_0("help", help_fn))
    .apply_mut(math::register)
    .apply_mut(strings::register)
    .apply_mut(lists::register)
    .apply_mut(structs::register);
}

/// Defines a symbol in the global scope.
fn define_fn(vm: &mut Vm, sym: Val, val: Val) -> VmResult<Val> {
    let sym = sym.as_symbol_id().ok_or_else(|| VmError::WrongType)?;
    vm.set_global(sym, val);
    Ok(Val::Void)
}

fn apply_fn(vm: &mut Vm, f: Val, args: Val) -> VmResult<Val> {
    let args = args.as_list(vm).ok_or_else(|| VmError::WrongType)?.clone();
    vm.eval_function(f, &args)
}

/// Returns the last value in the list of arguments.
fn do_fn(_: &Vm, args: &[Val]) -> VmResult<Val> {
    Ok(args.last().copied().unwrap_or(Val::Void))
}

/// Throw an error.
fn throw_fn(_: &Vm, _: &[Val]) -> VmResult<Val> {
    Err(VmError::Custom("exception thrown".into()))
}

fn val_to_string_fn(vm: &mut Vm, val: Val) -> VmResult<Val> {
    let s = format_compact!("{}", val.formatted(vm));
    Ok(vm.make_string(s))
}

fn val_to_type_fn(_: &mut Vm, val: Val) -> VmResult<Val> {
    Ok(Val::DataType(val.spore_type()))
}

fn equal_fn_impl(vm: &Vm, a: Val, b: Val) -> bool {
    match (a, b) {
        (Val::Void, Val::Void) => true,
        (Val::Bool(a), Val::Bool(b)) => a == b,
        (Val::Int(a), Val::Int(b)) => a == b,
        (Val::Float(a), Val::Float(b)) => a == b,
        (Val::Symbol(a), Val::Symbol(b)) => a == b,
        (Val::String(a), Val::String(b)) => {
            a == b || vm.objects.get_str(a) == vm.objects.get_str(b)
        }
        (Val::ShortString(a), Val::ShortString(b)) => a == b,
        (Val::List(a_list_id), Val::List(b_list_id)) => {
            a_list_id == b_list_id
                || match (
                    vm.objects.get_list(a_list_id),
                    vm.objects.get_list(b_list_id),
                ) {
                    (Some(a_list), Some(b_list)) if a_list.len() == b_list.len() => a_list
                        .iter()
                        .zip(b_list.iter())
                        .all(|(a_item, b_item)| equal_fn_impl(vm, *a_item, *b_item)),
                    _ => false,
                }
        }
        (Val::Struct(a_struct_id), Val::Struct(b_struct_id)) => {
            a_struct_id == b_struct_id
                || match (
                    vm.objects.get_struct(a_struct_id),
                    vm.objects.get_struct(b_struct_id),
                ) {
                    (Some(a_struct), Some(b_struct)) if a_struct.len() == b_struct.len() => {
                        a_struct
                            .iter()
                            .all(|(a_key, a_val)| match b_struct.get(a_key) {
                                Some(b_val) => equal_fn_impl(vm, *a_val, *b_val),
                                None => false,
                            })
                    }
                    _ => false,
                }
        }
        (Val::NativeFunction(a), Val::NativeFunction(b)) => a == b,
        (Val::BytecodeFunction(a), Val::BytecodeFunction(b)) => a == b,
        (Val::Custom(a), Val::Custom(b)) => a == b,
        (Val::DataType(a), Val::DataType(b)) => a == b,
        _ => false,
    }
}

fn equal_fn(vm: &mut Vm, a: Val, b: Val) -> VmResult<Val> {
    Ok(Val::Bool(equal_fn_impl(vm, a, b)))
}

/// Print the help.
fn help_fn(vm: &mut Vm) -> VmResult<Val> {
    let mut symbols: SporeList = vm.globals.values.keys().map(|k| Val::Symbol(*k)).collect();
    symbols.sort_by_key(|k| {
        k.as_symbol_id()
            .map(|id| vm.objects.symbols.symbol_name(id).unwrap_or(""))
    });
    let val = Val::List(vm.objects.register_list(symbols));
    Ok(val)
}

#[cfg(test)]
mod tests {
    use crate::{
        register_spore_type,
        val::{functions::NativeFunction, Val},
        vm::{Vm, VmError},
    };

    #[derive(Debug)]
    struct TestCustomType;
    register_spore_type!(TestCustomType);

    #[test]
    fn define_binds_values_to_name() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(%define 'x 12)"), Ok(Val::Void));
        assert_eq!(vm.clean_eval_str("x"), Ok(Val::Int(12)));
    }

    #[test]
    fn apply_applies_function_with_args_from_list() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(apply + (list 1 2 3 4))"),
            Ok(Val::Int(10))
        );
        assert_eq!(
            vm.clean_eval_str(
                r#"
(+
    (apply + (list 1 2 3 4))
    (apply - (list 4 3 2 1)))
"#
            ),
            Ok(Val::Int(8))
        );
    }

    #[test]
    fn do_returns_last_value_or_void_if_empty() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(do)"), Ok(Val::Void));
        assert_eq!(vm.clean_eval_str("(do 1)"), Ok(Val::Int(1)));
        assert_eq!(vm.clean_eval_str("(do 1 2)"), Ok(Val::Int(2)));
    }

    #[test]
    fn eq_with_equal_values_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(= false false)"), Ok(Val::Bool(true)));
        assert_eq!(vm.clean_eval_str("(= 1 1)"), Ok(Val::Bool(true)));
        assert_eq!(vm.clean_eval_str("(= 2.0 2.0)"), Ok(Val::Bool(true)));
        assert_eq!(
            vm.clean_eval_str("(= \"shortstr\" \"shortstr\")"),
            Ok(Val::Bool(true))
        );
        assert_eq!(
            vm.clean_eval_str("(= \"longer-str-is-eq-too\" \"longer-str-is-eq-too\")"),
            Ok(Val::Bool(true))
        );
        assert_eq!(
            vm.clean_eval_str("(= 'a-symbol 'a-symbol)"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_different_values_returns_false() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(= false true)"), Ok(Val::Bool(false)));
        assert_eq!(vm.clean_eval_str("(= 1 1.0)"), Ok(Val::Bool(false)));
        assert_eq!(
            vm.clean_eval_str("(= 'symbol \"string\")"),
            Ok(Val::Bool(false))
        );
        assert_eq!(vm.clean_eval_str("(= 1 2)"), Ok(Val::Bool(false)));
        assert_eq!(vm.clean_eval_str("(= 2.0 3.0)"), Ok(Val::Bool(false)));
        assert_eq!(vm.clean_eval_str("(= \"a\" \"b\")"), Ok(Val::Bool(false)));
        assert_eq!(
            vm.clean_eval_str("(= \"longer-str-is-not-eq-to\" \"short\")"),
            Ok(Val::Bool(false))
        );
        assert_eq!(
            vm.clean_eval_str(
                "(= \"longer-str-is-not-eq-to\" \"this-also-long-string-that-is-not-a-short-str\")"
            ),
            Ok(Val::Bool(false))
        );
        assert_eq!(
            vm.clean_eval_str("(= 'a-symbol 'b-symbol)"),
            Ok(Val::Bool(false))
        );
    }

    #[test]
    fn eq_with_void_returns_true() {
        let mut vm = Vm::default();
        vm.register_native_function(NativeFunction::new("new-void", |_| Ok(Val::Void)));
        assert_eq!(
            vm.clean_eval_str("(= (new-void) (new-void))"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_same_lists_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(= (list 1 2 3) (list 1 2 3))"),
            Ok(Val::Bool(true))
        );
        vm.clean_eval_str("(define eq-list (list 4 5 6))").unwrap();
        assert_eq!(
            vm.clean_eval_str("(= eq-list eq-list)"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_different_lists_returns_false() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(= (list) (list 1))"),
            Ok(Val::Bool(false))
        );
        assert_eq!(
            vm.clean_eval_str("(= (list 1 2 3) (list 4 5 6))"),
            Ok(Val::Bool(false))
        );
    }

    #[test]
    fn eq_with_same_structs_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(= (struct 'a 1 'b 2) (struct 'b 2 'a 1))"),
            Ok(Val::Bool(true))
        );
        vm.clean_eval_str("(define eq-struct (struct 'a 10 'b 20))")
            .unwrap();
        assert_eq!(
            vm.clean_eval_str("(= eq-struct eq-struct)"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_different_structs_returns_false() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(= (struct 'a 1 'b 2) (struct 'c 1 'd 2))"),
            Ok(Val::Bool(false))
        );
        assert_eq!(
            vm.clean_eval_str("(= (struct 'a 1) (struct 'a 2))"),
            Ok(Val::Bool(false))
        );
    }

    #[test]
    fn eq_with_same_custom_type_returns_true() {
        let mut vm = Vm::default();
        let val = vm.make_custom(TestCustomType);
        vm.set_global_by_name("custom-val", val);
        assert_eq!(
            vm.clean_eval_str("(= custom-val custom-val)"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_different_but_equivalent_custom_type_returns_false() {
        let mut vm = Vm::default();
        let val1 = vm.make_custom(TestCustomType);
        let val2 = vm.make_custom(TestCustomType);
        vm.set_global_by_name("custom-val1", val1);
        vm.set_global_by_name("custom-val2", val2);
        assert_eq!(
            vm.clean_eval_str("(= custom-val1 custom-val2)"),
            Ok(Val::Bool(false))
        );
    }

    #[test]
    fn eq_with_same_function_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(= = =)"), Ok(Val::Bool(true)));
        vm.clean_eval_str("(define (function-foo) 42)").unwrap();
        assert_eq!(
            vm.clean_eval_str("(= function-foo function-foo)"),
            Ok(Val::Bool(true))
        );
    }

    #[test]
    fn eq_with_different_function_returns_true() {
        let mut vm = Vm::default();
        assert_eq!(vm.clean_eval_str("(= = <)"), Ok(Val::Bool(false)));
        vm.clean_eval_str("(define (function-foo) 42)").unwrap();
        vm.clean_eval_str("(define (function-bar) 42)").unwrap();
        assert_eq!(
            vm.clean_eval_str("(= function-foo function-bar)"),
            Ok(Val::Bool(false))
        );
        assert_eq!(
            vm.clean_eval_str("(= function-foo <)"),
            Ok(Val::Bool(false))
        );
    }

    #[test]
    fn throw_returns_an_error() {
        let mut vm = Vm::default();
        assert_eq!(
            vm.clean_eval_str("(throw)"),
            Err(VmError::Custom("exception thrown".into()))
        );
        assert_eq!(
            vm.clean_eval_str("(throw 1)"),
            Err(VmError::Custom("exception thrown".into()))
        );
        assert_eq!(
            vm.clean_eval_str("(throw 1 2 3)"),
            Err(VmError::Custom("exception thrown".into()))
        );
    }
}
