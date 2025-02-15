use compact_str::ToCompactString;
use ropey::Rope;
use spore_vm::{
    register_spore_type,
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError},
};

#[derive(Debug)]
pub struct Buffer {
    text: Rope,
}

register_spore_type!(Buffer);

pub fn register_buffer(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "new-buffer",
        |vm: &mut Vm, text: Val| {
            let text = text.as_str(vm).ok_or(VmError::WrongType)?;
            let buffer = Buffer {
                text: Rope::from_str(text),
            };
            let val = vm.make_custom(buffer);
            Ok(val)
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "buffer-append!",
        move |vm: &mut Vm, buffer: Val, text: Val| {
            let text = text
                .as_str(vm)
                .ok_or(VmError::WrongType)?
                .to_compact_string();
            let buffer: &mut Buffer = buffer.as_custom_mut(vm).ok_or(VmError::WrongType)?;
            buffer.text.insert(buffer.text.len(), &text);
            Ok(Val::Void)
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "buffer->string",
        |vm: &mut Vm, buffer: Val| {
            let buffer: &Buffer = buffer.as_custom(vm).ok_or(VmError::WrongType)?;
            Ok(vm.make_string(buffer.text.to_string()))
        },
    ));
}
