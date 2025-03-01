use crate::{
    error::{VmError, VmResult},
    val::{native_function::NativeFunction, DataType, Val},
    vm::Vm,
};

pub fn register(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1("string-len", string_len_fn));
}

fn string_len_fn(vm: &mut Vm, s: Val) -> VmResult<Val> {
    let s = s.as_str(vm).ok_or_else(|| VmError::WrongType {
        expected: DataType::String,
        actual: s.spore_type(),
    })?;
    Ok(Val::Int(s.len() as i64))
}
