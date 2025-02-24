use std::{marker::PhantomData, num::NonZeroU32};

/// An identifier for an object in the object store.
pub struct ObjectId<T>(NonZeroU32, PhantomData<T>);

impl<T> ObjectId<T> {
    /// Get the id as a number.
    pub fn as_num(self) -> u32 {
        u32::from(self.0)
    }
}

impl<T> From<NonZeroU32> for ObjectId<T> {
    fn from(num: NonZeroU32) -> ObjectId<T> {
        ObjectId(num, PhantomData)
    }
}

impl<T> Eq for ObjectId<T> {}
impl<T> Copy for ObjectId<T> {}
impl<T> Clone for ObjectId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for ObjectId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> std::hash::Hash for ObjectId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
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
