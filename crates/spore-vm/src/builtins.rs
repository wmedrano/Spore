mod lists;
mod math;
mod structs;

/// The internal symbol used to define a new value.
pub const INTERNAL_DEFINE_FUNCTION: &str = "%define";

use compact_str::format_compact;

use crate::{
    val::{functions::NativeFunction, DataType, Val},
    vm::{Vm, VmError, VmResult},
    SporeList, SporeStruct,
};

/// Registers the built-in functions in the VM.
pub fn register_builtins(vm: &mut Vm) -> &mut Vm {
    vm.register_native_function(NativeFunction::with_args_2(
        INTERNAL_DEFINE_FUNCTION,
        define_fn,
    ))
    .register_native_function(NativeFunction::with_arg_list("do", do_fn))
    .register_native_function(NativeFunction::with_arg_list("throw", throw_fn))
    .register_native_function(NativeFunction::with_args_1("val->string", val_to_string_fn))
    .register_native_function(NativeFunction::with_args_1("val->type", val_to_type_fn))
    .register_native_function(NativeFunction::with_args_0("help", help_fn));
    math::register(vm);
    lists::register(vm);
    structs::register(vm);
    vm
}

/// Defines a symbol in the global scope.
fn define_fn(vm: &mut Vm, sym: Val, val: Val) -> VmResult<Val> {
    let global_id = vm.common_symbols.global;
    let sym = sym.as_symbol_id().ok_or_else(|| VmError::WrongType)?;
    vm.modules
        .get_mut(&global_id)
        .expect("%global module not found")
        .values
        .insert(sym, val);
    Ok(Val::Void)
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
    let t = match val {
        Val::Void => DataType::Void,
        Val::Bool(_) => DataType::Bool,
        Val::Int(_) => DataType::Int,
        Val::Float(_) => DataType::Float,
        Val::Symbol(_) => DataType::Symbol,
        Val::String(_) => DataType::String,
        Val::List(_) => DataType::List,
        Val::Struct(_) => DataType::StructT,
        Val::NativeFunction(_) => DataType::Function,
        Val::BytecodeFunction(_) => DataType::Function,
        Val::Custom(_) => DataType::Custom,
        Val::DataType(_) => DataType::DataType,
    };
    Ok(Val::DataType(t))
}

/// Print the help.
fn help_fn(vm: &mut Vm) -> VmResult<Val> {
    let ret = SporeStruct::from_iter(vm.modules.iter().map(|(module_name, module)| {
        let mut symbols: SporeList = module.values.keys().map(|k| Val::Symbol(*k)).collect();
        symbols.sort_by_key(|k| {
            k.as_symbol_id()
                .map(|id| vm.objects.symbols.symbol_name(id).unwrap_or(""))
        });
        (*module_name, Val::List(vm.objects.register_list(symbols)))
    }));
    Ok(Val::Struct(vm.objects.register_struct(ret)))
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
