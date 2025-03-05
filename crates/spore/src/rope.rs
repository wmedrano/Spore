use compact_str::{format_compact, ToCompactString};
use crop::Rope;
use spore_vm::{
    error::VmError,
    register_spore_type,
    val::{native_function::NativeFunction, DataType, Val},
    vm::Vm,
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
            let text = text.as_str(vm).ok_or(VmError::WrongType {
                function_name: "new-rope".into(),
                expected: DataType::String,
                actual: text.spore_type(),
            })?;
            Ok(vm.make_custom(SporeRope::new(text)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "new-rope-with-file",
        |vm: &mut Vm, filename: Val| {
            let filename = filename.as_str(vm).ok_or(VmError::WrongType {
                function_name: "new-rope-with-file".into(),
                expected: DataType::String,
                actual: filename.spore_type(),
            })?;
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
        |vm: &mut Vm, buffer: Val| {
            let rope: &SporeRope = buffer.as_custom(vm).ok_or(VmError::WrongType {
                function_name: "rope-len".into(),
                expected: DataType::Custom,
                actual: buffer.spore_type(),
            })?;
            Ok(Val::Int(rope.inner.byte_len() as i64))
        },
    ))
    .register_native_function(NativeFunction::with_args_3(
        "rope-insert!",
        |vm: &mut Vm, buffer: Val, pos: Val, text: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType {
                function_name: "rope-insert!".into(),
                expected: DataType::Int,
                actual: pos.spore_type(),
            })? as usize;
            let text = text.as_str(vm).ok_or(VmError::WrongType {
                function_name: "rope-insert!".into(),
                expected: DataType::String,
                actual: text.spore_type(),
            })?;
            let insert_len = {
                // We extend the lifetime so that we can borrow the rope mutably. This is ok as we
                // aren't modifying any layout, just the rope reference itself.
                let unsafe_text: &'static str = unsafe { std::mem::transmute(text) };
                let buffer: &mut SporeRope =
                    buffer.as_custom_mut(vm).ok_or(VmError::WrongType {
                        function_name: "rope-insert!".into(),
                        expected: DataType::Custom,
                        actual: buffer.spore_type(),
                    })?;
                buffer.insert(pos, unsafe_text) as i64
            };
            Ok(Val::Int(insert_len))
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "rope-delete!",
        |vm: &mut Vm, buffer: Val, pos: Val| {
            let pos = pos.as_int().ok_or(VmError::WrongType {
                function_name: "rope-delete!".into(),
                expected: DataType::Int,
                actual: pos.spore_type(),
            })? as usize;
            let buffer: &mut SporeRope = buffer.as_custom_mut(vm).ok_or(VmError::WrongType {
                function_name: "rope-delete!".into(),
                expected: DataType::Custom,
                actual: buffer.spore_type(),
            })?;
            buffer.inner.delete(pos..pos + 1);
            Ok(Val::Void)
        },
    ))
    .register_native_function(NativeFunction::with_args_2(
        "rope-normalize-cursor",
        |vm: &mut Vm, rope: Val, cursor: Val| {
            let rope_len = rope
                .as_custom::<SporeRope>(vm)
                .ok_or(VmError::WrongType {
                    function_name: "rope-normalize-cursor".into(),
                    expected: DataType::Custom,
                    actual: rope.spore_type(),
                })?
                .inner
                .byte_len();
            let pos = cursor.as_int().ok_or(VmError::WrongType {
                function_name: "rope-normalize-cursor".into(),
                expected: DataType::Int,
                actual: cursor.spore_type(),
            })?;
            Ok(Val::Int(pos.clamp(0, rope_len as i64)))
        },
    ))
    .register_native_function(NativeFunction::with_args_1(
        "rope->string",
        |vm: &mut Vm, rope: Val| {
            let rope: &SporeRope = rope.as_custom(vm).ok_or(VmError::WrongType {
                function_name: "rope->string".into(),
                expected: DataType::Custom,
                actual: rope.spore_type(),
            })?;
            Ok(vm.make_string(rope.inner.to_compact_string()))
        },
    ));
}
