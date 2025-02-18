use compact_str::format_compact;
use spore_vm::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError},
};

pub fn register_files(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "file-read",
        |vm: &mut Vm, filename: Val| {
            let filename = filename.as_str(vm).ok_or_else(|| VmError::WrongType)?;
            let contents = std::fs::read_to_string(filename)
                .map_err(|err| VmError::Custom(format_compact!("{err}")))?;
            Ok(vm.make_string(contents))
        },
    ));
}
