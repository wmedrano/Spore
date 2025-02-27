use compact_str::format_compact;
use spore_vm::{
    val::{native_function::NativeFunction, Val},
    vm::{Vm, VmErrorInner},
};

pub fn register_files(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "file-read",
        |vm: &mut Vm, filename: Val| {
            let filename = filename.as_str(vm).ok_or_else(|| VmErrorInner::WrongType)?;
            let contents = std::fs::read_to_string(filename)
                .map_err(|err| VmErrorInner::Custom(format_compact!("{err}")))?;
            Ok(vm.make_string(contents))
        },
    ));
}
