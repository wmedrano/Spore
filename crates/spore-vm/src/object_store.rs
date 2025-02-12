use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use crate::{
    instruction::Instruction,
    module::Module,
    val::{
        functions::{ByteCodeFunction, NativeFunction},
        symbol::SymbolTable,
        Val,
    },
    vm::StackFrame,
};

/// An identifier for an object in the object store.
pub struct ObjectId<T>(u32, PhantomData<T>);

impl<T> ObjectId<T> {
    /// Get the id as a number.
    pub fn as_num(self) -> u32 {
        self.0
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
    next_id: u32,
}

impl<T> std::fmt::Debug for ObjectId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectId")
            .field(&std::any::type_name::<T>())
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
            next_id: 1,
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
        self.next_id += 1;
        self.id_to_object
            .insert(id, ColoredObject { object, color });
        id
    }

    /// Sweeps objects of a specific color from the store.
    pub fn sweep_color(&mut self, color: GcColor) {
        self.id_to_object.retain(|_, v| v.color != color)
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
    /// The store for native functions.
    pub native_functions: TypedObjectStore<NativeFunction>,
    /// The store for bytecode functions.
    pub bytecode_functions: TypedObjectStore<ByteCodeFunction>,
    /// The symbol table.
    pub symbols: SymbolTable,
    /// The null bytecode function.
    pub null_bytecode: ByteCodeFunction,
}

impl Objects {
    /// Runs garbage collection.
    ///
    /// This function initiates a garbage collection cycle, marking reachable objects and then sweeping unreachable objects.
    pub fn run_gc<'a>(
        &mut self,
        stack: &[Val],
        stack_frames: impl Iterator<Item = &'a StackFrame>,
        modules: impl Iterator<Item = &'a Module>,
    ) {
        self.mark(stack, stack_frames, modules);
        self.sweep();
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
            for instruction in frame.iter_instructions() {
                Self::mark_instruction(instruction, &mut queue);
            }
        }
        self.exhaust_queue(&mut queue, &mut tmp_queue);
    }

    fn exhaust_queue(&mut self, queue: &mut Vec<Val>, tmp_queue: &mut Vec<Val>) {
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
            Val::Void | Val::Bool(_) | Val::Int(_) | Val::Float(_) | Val::Symbol(_) => (),
        }
    }

    fn mark_instruction(instruction: &Instruction, queue: &mut Vec<Val>) {
        match instruction {
            Instruction::Push(v) => queue.push(*v),
            Instruction::Eval(_)
            | Instruction::Get(_)
            | Instruction::Deref(_)
            | Instruction::Return => {}
        }
    }

    /// Sweeps the object store to collect garbage.
    ///
    /// This function sweeps the object store, collecting any objects that are not marked with the current reachable color.
    fn sweep(&mut self) {
        self.reachable_color = self.reachable_color.swap();
        self.native_functions.sweep_color(self.reachable_color);
        self.bytecode_functions.sweep_color(self.reachable_color);
    }
}
