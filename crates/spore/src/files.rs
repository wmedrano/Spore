use compact_str::format_compact;
use spore_vm::{
    error::VmError,
    val::{native_function::NativeFunction, DataType, Val},
    vm::Vm,
};

pub fn register_files(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "file-read",
        |vm: &mut Vm, filename: Val| {
            let filename = filename.as_str(vm).ok_or_else(|| VmError::WrongType {
                function_name: "file-read".into(),
                expected: DataType::String,
                actual: filename.spore_type(),
            })?;
            let contents = std::fs::read_to_string(filename)
                .map_err(|err| VmError::Custom(format_compact!("{err}")))?;
            Ok(vm.make_string(contents))
        },
    ));
}
