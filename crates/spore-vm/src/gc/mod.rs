pub use id::ObjectId;

use compact_str::CompactString;
use typed_object_store::{GcColor, TypedObjectStore};

use crate::{
    module::Module,
    val::{
        bytecode_function::ByteCodeFunction, custom::SporeCustom, native_function::NativeFunction,
        symbol::SymbolTable, Val,
    },
    vm::StackFrame,
    SporeCustomType, SporeList, SporeStruct,
};

mod id;
mod typed_object_store;

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
            if let Some(id) = frame.function_id() {
                self.mark_many(
                    std::iter::once(Val::BytecodeFunction { id, captures: None }),
                    &mut queue,
                    &mut tmp_queue,
                );
            }
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
        let has_gc = match val {
            Val::String(id) => {
                self.strings.maybe_color(id, self.reachable_color);
                true
            }
            Val::List(id) => {
                if let Some(lst) = self.lists.maybe_color(id, self.reachable_color) {
                    for v in lst.iter().filter(|v| v.requires_gc()) {
                        queue.push(*v);
                    }
                }
                true
            }
            Val::Struct(id) => {
                if let Some(strct) = self.structs.maybe_color(id, self.reachable_color) {
                    for v in strct.values().filter(|v| v.requires_gc()) {
                        queue.push(*v);
                    }
                }
                true
            }
            Val::NativeFunction(id) => {
                self.native_functions.maybe_color(id, self.reachable_color);
                true
            }
            Val::BytecodeFunction { id, captures } => {
                if let Some(f) = self
                    .bytecode_functions
                    .maybe_color(id, self.reachable_color)
                {
                    queue.extend(f.iter_references().filter(|v| v.requires_gc()));
                }
                if let Some(lst) = captures {
                    queue.push(Val::List(lst));
                }
                true
            }
            Val::Custom(id) => {
                if let Some(obj) = self.custom.maybe_color(id, self.reachable_color) {
                    for val in obj.references().iter().filter(|v| v.requires_gc()) {
                        queue.push(*val);
                    }
                }
                true
            }
            Val::Box(id) => {
                if let Some(v) = self.boxes.maybe_color(id, self.reachable_color) {
                    if v.requires_gc() {
                        queue.push(*v);
                    }
                }
                true
            }
            Val::Void
            | Val::Bool(_)
            | Val::Int(_)
            | Val::Float(_)
            | Val::Symbol(_)
            | Val::ShortString(_)
            | Val::DataType(_) => false,
        };
        debug_assert_eq!(has_gc, val.requires_gc(), "{val:?}");
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
            + self.boxes.sweep_color(self.reachable_color)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn object_id_is_small() {
        assert_eq!(std::mem::size_of::<ObjectId<()>>(), 4);
        assert_eq!(std::mem::size_of::<Option<ObjectId<()>>>(), 4);
    }
}
