use compact_str::{format_compact, ToCompactString};
use crop::Rope;
use spore_vm::{
    register_spore_type,
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmError},
};

#[derive(Debug)]
pub struct SporeRope {
    pub(crate) inner: Rope,
}

register_spore_type!(SporeRope);

impl SporeRope {
    pub fn new(text: &str) -> SporeRope {
        SporeRope {
            inner: Rope::from(text),
        }
    }

    pub fn insert(&mut self, pos: usize, text: &str) -> usize {
        self.inner.insert(pos, text);
        text.len()
    }
}

pub fn register_rope(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::with_args_1(
        "new-rope",
        |vm: &mut Vm, text: Val| {
            let text = text.as_str(vm).ok_or(VmError::WrongType)?;
            Ok(vm.make_custom(SporeRope::new(text)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "new-rope-with-file",
        |vm: &mut Vm, filename: Val| {
            let filename = filename.as_str(vm).ok_or(VmError::WrongType)?;
            let file_contents = std::fs::read_to_string(filename).map_err(|err| {
                VmError::Custom(format_compact!(
                    "Failed to read file {filename:?}: {err}",
                    filename = filename
                ))
            })?;
            Ok(vm.make_custom(SporeRope::new(&file_contents)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "rope-len",
        move |vm: &mut Vm, buffer: Val| {
            let rope: &SporeRope = buffer.as_custom(vm).ok_or(VmError::WrongType)?;
            Ok(Val::Int(rope.inner.byte_len() as i64))
        },
    ))
    .register_native_function(NativeFunction::with_args_3(
        "rope-insert!",
        move |vm: &mut Vm, buffer: Val, pos: Val, text: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType)? as usize;
            let text = text
                .as_str(vm)
                .ok_or(VmError::WrongType)?
                .to_compact_string();
            let buffer: &mut SporeRope = buffer.as_custom_mut(vm).ok_or(VmError::WrongType)?;
            Ok(Val::Int(buffer.insert(pos, &text) as i64))
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "rope-delete!",
        move |vm: &mut Vm, buffer: Val, pos: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType)? as usize;
            let buffer: &mut SporeRope = buffer.as_custom_mut(vm).ok_or(VmError::WrongType)?;
            buffer.inner.delete(pos..pos + 1);
            Ok(Val::Void)
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "rope-normalize-cursor",
        move |vm: &mut Vm, rope: Val, cursor: Val| {
            let rope_len = rope
                .as_custom::<SporeRope>(vm)
                .ok_or(VmError::WrongType)?
                .inner
                .byte_len();
            let pos = cursor.as_int().ok_or(VmError::WrongType)?;
            Ok(Val::Int(pos.clamp(0, rope_len as i64)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "rope->string",
        |vm: &mut Vm, rope: Val| {
            let rope: &SporeRope = rope.as_custom(vm).ok_or(VmError::WrongType)?;
            Ok(vm.make_string(rope.inner.to_string()))
        },
    ));
}
