use std::marker::PhantomData;

/// An identifier for an object in the object store.
pub struct ObjectId<T>(u32, PhantomData<T>);

const TAG_BITS: usize = 8;
const IDX_BITS: usize = 32 - TAG_BITS;
const IDX_MASK: u32 = 0xffffffff >> TAG_BITS;
const TAG_MASK: u32 = 0xffffffff << (32 - TAG_BITS);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Tag(u8);

impl<T> ObjectId<T> {
    pub fn new(tag: Tag, idx: usize) -> ObjectId<T> {
        let tag_part = (tag.0 as u32) << IDX_BITS;
        let idx_part = idx as u32;
        ObjectId(tag_part | idx_part, PhantomData)
    }

    pub fn num(self) -> u32 {
        self.0
    }

    /// Get the id as a number.
    pub fn idx(self) -> usize {
        let idx: u32 = self.0 & IDX_MASK;
        idx as usize
    }

    pub fn tag(self) -> Tag {
        let tag = (self.0 & TAG_MASK) >> IDX_BITS;
        Tag(tag as u8)
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

impl Tag {
    pub fn increment(self) -> Tag {
        Tag(self.0.wrapping_add(1))
    }
}
