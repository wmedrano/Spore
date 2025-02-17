use compact_str::ToCompactString;
use crop::Rope;
use spore_vm::{
    register_spore_type,
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError},
};

#[derive(Debug)]
pub struct TextBuffer {
    pub(crate) text: Rope,
}

register_spore_type!(TextBuffer);

impl TextBuffer {
    pub fn new(text: &str) -> TextBuffer {
        TextBuffer {
            text: Rope::from(text),
        }
    }

    pub fn insert(&mut self, pos: usize, text: &str) -> usize {
        self.text.insert(pos, text);
        text.len()
    }
}

pub fn register_buffer(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "new-buffer",
        |vm: &mut Vm, text: Val| {
            let text = text.as_str(vm).ok_or(VmError::WrongType)?;
            Ok(vm.make_custom(TextBuffer::new(text)))
        },
    ))
    .register_native_function(NativeFunction::with_args_3(
        "buffer-insert!",
        move |vm: &mut Vm, buffer: Val, pos: Val, text: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType)? as usize;
            let text = text
                .as_str(vm)
                .ok_or(VmError::WrongType)?
                .to_compact_string();
            let buffer: &mut TextBuffer = buffer.as_custom_mut(vm).ok_or(VmError::WrongType)?;
            Ok(Val::Int(buffer.insert(pos, &text) as i64))
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "buffer-delete!",
        move |vm: &mut Vm, buffer: Val, pos: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType)? as usize;
            let buffer: &mut TextBuffer = buffer.as_custom_mut(vm).ok_or(VmError::WrongType)?;
            buffer.text.delete(pos..pos + 1);
            Ok(Val::Void)
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "buffer-normalize-cursor",
        move |vm: &mut Vm, buffer: Val, cursor: Val| {
            let buffer_len = buffer
                .as_custom::<TextBuffer>(vm)
                .ok_or(VmError::WrongType)?
                .text
                .byte_len();
            let pos = cursor.as_int().ok_or(VmError::WrongType)?;
            Ok(Val::Int(pos.clamp(0, buffer_len as i64)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "buffer->string",
        |vm: &mut Vm, buffer: Val| {
            let buffer: &TextBuffer = buffer.as_custom(vm).ok_or(VmError::WrongType)?;
            Ok(vm.make_string(buffer.text.to_string()))
        },
    ));
}
