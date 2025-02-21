use std::{collections::HashMap, hash::Hash, marker::PhantomData, num::NonZeroU32};

use compact_str::CompactString;

use crate::{
    instruction::Instruction,
    module::Module,
    val::{
        custom::SporeCustom,
        functions::{ByteCodeFunction, NativeFunction},
        symbol::SymbolTable,
        Val,
    },
    vm::StackFrame,
    SporeCustomType, SporeList, SporeStruct,
};

/// An identifier for an object in the object store.
pub struct ObjectId<T>(NonZeroU32, PhantomData<T>);

impl<T> ObjectId<T> {
    /// Get the id as a number.
    pub fn as_num(self) -> u32 {
        u32::from(self.0)
    }
}

impl<T> PartialEq for ObjectId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
/// A colored object.
pub struct ColoredObject<T> {
    object: T,
    color: GcColor,
}

/// Represents a garbage collection color.
#[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
pub enum GcColor {
    #[default]
    Red,
    Blue,
}

#[derive(Debug)]
/// A store for objects of a specific type.
pub struct TypedObjectStore<T> {
    id_to_object: HashMap<ObjectId<T>, ColoredObject<T>>,
    next_id: NonZeroU32,
}

impl<T> std::fmt::Debug for ObjectId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let full_type_name = std::any::type_name::<T>();
        let type_name = full_type_name.split("::").last().unwrap_or("UNKNOWN");
        f.debug_tuple("ObjectId")
            .field(&type_name)
            .field(&self.0)
            .finish()
    }
}

impl<T> Eq for ObjectId<T> {}
impl<T> Copy for ObjectId<T> {}
impl<T> Clone for ObjectId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Hash for ObjectId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> Default for TypedObjectStore<T> {
    fn default() -> Self {
        TypedObjectStore {
            id_to_object: HashMap::new(),
            next_id: NonZeroU32::new(1).unwrap(),
        }
    }
}

impl<T> TypedObjectStore<T> {
    /// Maybe color an object, changing its color to the provided `color` if it is not already that color.
    ///
    /// If the object's color is already `color`, this function returns `None`. Otherwise, the
    /// object's color is updated to `color` and a reference to the object is returned in `Some`.
    pub fn maybe_color(&mut self, id: ObjectId<T>, color: GcColor) -> Option<&T> {
        let colored_object = self.id_to_object.get_mut(&id)?;
        if colored_object.color == color {
            return None;
        }
        colored_object.color = color;
        Some(&colored_object.object)
    }

    /// Gets an object by its ID.
    pub fn get(&self, id: ObjectId<T>) -> Option<&T> {
        self.id_to_object.get(&id).map(|v| &v.object)
    }

    /// Gets a mutable reference to an object by its ID.
    pub fn get_mut(&mut self, id: ObjectId<T>) -> Option<&mut T> {
        self.id_to_object.get_mut(&id).map(|v| &mut v.object)
    }

    /// Registers a new object in the store.
    pub fn register(&mut self, object: T, color: GcColor) -> ObjectId<T> {
        let id = ObjectId(self.next_id, PhantomData);
        self.next_id = self
            .next_id
            .checked_add(1)
            .expect("ObjectId limit reached, all u32 values exhausted");
        self.id_to_object
            .insert(id, ColoredObject { object, color });
        id
    }

    /// Sweeps objects of a specific color from the store.
    pub fn sweep_color(&mut self, color: GcColor) -> usize {
        let before = self.id_to_object.len();
        self.id_to_object.retain(|_, v| v.color != color);
        before - self.id_to_object.len()
    }
}

impl GcColor {
    /// Returns the opposite color.
    pub fn swap(self) -> GcColor {
        match self {
            GcColor::Red => GcColor::Blue,
            GcColor::Blue => GcColor::Red,
        }
    }
}

#[derive(Debug, Default)]
/// Stores all the heap allocated objects.
///
/// The Objects struct manages the heap allocated objects, including native functions,
/// bytecode functions, and symbols. It also handles garbage collection.
pub struct Objects {
    /// The color used to mark reachable objects during GC.
    pub reachable_color: GcColor,
    /// The store for strings.
    pub strings: TypedObjectStore<CompactString>,
    /// The store for lists.
    pub lists: TypedObjectStore<SporeList>,
    /// The store for structs.
    pub structs: TypedObjectStore<SporeStruct>,
    /// The store for native functions.
    pub native_functions: TypedObjectStore<NativeFunction>,
    /// The store for bytecode functions.
    pub bytecode_functions: TypedObjectStore<ByteCodeFunction>,
    /// The store for custom objects.
    pub custom: TypedObjectStore<SporeCustom>,
    /// The store for box objects.
    pub boxes: TypedObjectStore<Val>,
    /// The symbol table.
    pub symbols: SymbolTable,
    /// The null bytecode function.
    pub null_bytecode: ByteCodeFunction,
}

impl Objects {
    pub fn register_bytecode(&mut self, bc: ByteCodeFunction) -> ObjectId<ByteCodeFunction> {
        self.bytecode_functions
            .register(bc, self.reachable_color.swap())
    }

    pub fn register_string(&mut self, s: CompactString) -> ObjectId<CompactString> {
        self.strings.register(s, self.reachable_color.swap())
    }

    pub fn register_list(&mut self, lst: impl Into<SporeList>) -> ObjectId<SporeList> {
        self.lists.register(lst.into(), self.reachable_color.swap())
    }

    pub fn register_struct(&mut self, strct: impl Into<SporeStruct>) -> ObjectId<SporeStruct> {
        self.structs
            .register(strct.into(), self.reachable_color.swap())
    }

    pub fn register_custom(&mut self, custom: impl SporeCustomType) -> ObjectId<SporeCustom> {
        let custom = SporeCustom::new(custom);
        self.custom.register(custom, self.reachable_color.swap())
    }

    pub fn register_box(&mut self, v: Val) -> ObjectId<Val> {
        self.boxes.register(v, self.reachable_color.swap())
    }

    pub fn get_str(&self, string_id: ObjectId<CompactString>) -> Option<&str> {
        self.strings.get(string_id).map(CompactString::as_str)
    }

    pub fn get_list(&self, list_id: ObjectId<SporeList>) -> Option<&SporeList> {
        self.lists.get(list_id)
    }

    pub fn get_struct(&self, struct_id: ObjectId<SporeStruct>) -> Option<&SporeStruct> {
        self.structs.get(struct_id)
    }

    pub fn get_struct_mut(&mut self, struct_id: ObjectId<SporeStruct>) -> Option<&mut SporeStruct> {
        self.structs.get_mut(struct_id)
    }

    pub fn get_custom(&self, custom_id: ObjectId<SporeCustom>) -> Option<&SporeCustom> {
        self.custom.get(custom_id)
    }

    pub fn get_custom_mut(&mut self, custom_id: ObjectId<SporeCustom>) -> Option<&mut SporeCustom> {
        self.custom.get_mut(custom_id)
    }

    pub fn get_box(&self, box_id: ObjectId<Val>) -> Option<Val> {
        self.boxes.get(box_id).copied()
    }

    pub fn get_box_mut(&mut self, box_id: ObjectId<Val>) -> Option<&mut Val> {
        self.boxes.get_mut(box_id)
    }

    /// Runs garbage collection.
    ///
    /// This function initiates a garbage collection cycle, marking reachable objects and then sweeping unreachable objects.
    pub fn run_gc<'a>(
        &mut self,
        stack: &[Val],
        stack_frames: impl Iterator<Item = &'a StackFrame>,
        modules: impl Iterator<Item = &'a Module>,
    ) -> usize {
        self.mark(stack, stack_frames, modules);
        self.sweep()
    }

    /// Marks reachable objects during garbage collection.
    ///
    /// This function recursively marks all reachable objects, starting from the stack, stack frames, and globals.
    fn mark<'a>(
        &mut self,
        stack: &[Val],
        stack_frames: impl Iterator<Item = &'a StackFrame>,
        modules: impl Iterator<Item = &'a Module>,
    ) {
        let (mut queue, mut tmp_queue) = (Vec::new(), Vec::new());
        self.mark_many(stack.iter().copied(), &mut queue, &mut tmp_queue);
        for module in modules {
            self.mark_many(module.values.values().copied(), &mut queue, &mut tmp_queue);
        }
        for frame in stack_frames {
            self.mark_many(frame.function_val().into_iter(), &mut queue, &mut tmp_queue);
        }
        self.exhaust_queue(&mut queue, &mut tmp_queue);
    }

    /// Clears every item in `queue`.
    ///
    /// `tmp_queue` must be empty.
    fn exhaust_queue(&mut self, queue: &mut Vec<Val>, tmp_queue: &mut Vec<Val>) {
        assert!(tmp_queue.is_empty());
        while !queue.is_empty() {
            for v in queue.drain(..) {
                self.mark_one(v, tmp_queue);
            }
            std::mem::swap(queue, tmp_queue);
        }
    }

    fn mark_many(
        &mut self,
        vals: impl Iterator<Item = Val>,
        queue: &mut Vec<Val>,
        tmp_queue: &mut Vec<Val>,
    ) {
        for v in vals {
            self.mark_one(v, queue);
        }
        self.exhaust_queue(queue, tmp_queue)
    }

    /// Marks a single value as reachable.
    ///
    /// This function marks a single `Val` as reachable by setting the appropriate color in the object store.
    fn mark_one(&mut self, val: Val, queue: &mut Vec<Val>) {
        match val {
            Val::String(id) => {
                self.strings.maybe_color(id, self.reachable_color);
            }
            Val::List(id) => {
                if let Some(lst) = self.lists.maybe_color(id, self.reachable_color) {
                    for v in lst.iter() {
                        queue.push(*v);
                    }
                }
            }
            Val::Struct(id) => {
                if let Some(strct) = self.structs.maybe_color(id, self.reachable_color) {
                    for v in strct.values() {
                        queue.push(*v);
                    }
                }
            }
            Val::NativeFunction(id) => {
                self.native_functions.maybe_color(id, self.reachable_color);
            }
            Val::BytecodeFunction(id) => {
                if let Some(f) = self
                    .bytecode_functions
                    .maybe_color(id, self.reachable_color)
                {
                    for instruction in f.instructions.iter() {
                        Self::mark_instruction(instruction, queue);
                    }
                }
            }
            Val::Custom(id) => {
                if let Some(obj) = self.custom.maybe_color(id, self.reachable_color) {
                    for val in obj.references() {
                        queue.push(*val);
                    }
                }
            }
            Val::Box(id) => {
                if let Some(v) = self.boxes.maybe_color(id, self.reachable_color) {
                    queue.push(*v);
                }
            }
            Val::Void
            | Val::Bool(_)
            | Val::Int(_)
            | Val::Float(_)
            | Val::Symbol(_)
            | Val::ShortString(_)
            | Val::DataType(_) => (),
        }
    }

    fn mark_instruction(instruction: &Instruction, queue: &mut Vec<Val>) {
        match instruction {
            Instruction::Push(v) => queue.push(*v),
            Instruction::Return
            | Instruction::Eval(_)
            | Instruction::Get(_)
            | Instruction::Deref(_)
            | Instruction::Jump(_)
            | Instruction::JumpIf(_) => {}
        }
    }

    /// Sweeps the object store to collect garbage.
    ///
    /// This function sweeps the object store, collecting any objects that are not marked with the current reachable color.
    fn sweep(&mut self) -> usize {
        self.reachable_color = self.reachable_color.swap();
        self.strings.sweep_color(self.reachable_color)
            + self.lists.sweep_color(self.reachable_color)
            + self.structs.sweep_color(self.reachable_color)
            + self.native_functions.sweep_color(self.reachable_color)
            + self.bytecode_functions.sweep_color(self.reachable_color)
            + self.custom.sweep_color(self.reachable_color)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn object_id_is_small() {
        assert_eq!(
            std::mem::size_of::<ObjectId<()>>(),
            std::mem::size_of::<u32>()
        );
        assert_eq!(
            std::mem::size_of::<Option<ObjectId<()>>>(),
            std::mem::size_of::<u32>()
        );
    }
}
