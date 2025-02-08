use std::{collections::HashMap, hash::Hash, marker::PhantomData};

/// An identifier for an object in the object store.
pub struct ObjectId<T>(u32, PhantomData<T>);

impl<T> PartialEq for ObjectId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
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
        ObjectId(self.0, PhantomData)
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
