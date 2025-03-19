use std::mem::MaybeUninit;

use super::{id::Tag, ObjectId};

#[derive(Debug)]
/// A store for objects of a specific type.
pub struct TypedObjectStore<T> {
    objects: Vec<MaybeUninit<T>>,
    metadata: Vec<Metadata>,
    free_idx: Vec<usize>,
}

#[derive(Debug)]
/// A colored object.
pub struct Metadata {
    tag: Tag,
    color: GcColor,
}

/// Represents a garbage collection color.
#[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
pub enum GcColor {
    #[default]
    Red,
    Blue,
    Tombstone,
}

impl GcColor {
    /// Returns the opposite color.
    pub fn swap(self) -> GcColor {
        match self {
            GcColor::Red => GcColor::Blue,
            GcColor::Blue => GcColor::Red,
	    GcColor::Tombstone => GcColor::Tombstone,
        }
    }
}

impl<T> Default for TypedObjectStore<T> {
    fn default() -> Self {
        TypedObjectStore {
	    objects: Vec::new(),
	    metadata: Vec::new(),
	    free_idx: Vec::new(),
        }
    }
}

impl<T> TypedObjectStore<T> {
    /// Maybe color an object, changing its color to the provided `color` if it is not already that color.
    ///
    /// If the object's color is already `color`, this function returns `None`. Otherwise, the
    /// object's color is updated to `color` and a reference to the object is returned in `Some`.
    pub fn maybe_color(&mut self, id: ObjectId<T>, color: GcColor) -> Option<&T> {
	let idx = id.idx();
	let metadata = self.metadata.get_mut(idx)?;
	if metadata.color == color || metadata.color == GcColor::Tombstone {
	    return None;
	}
        metadata.color = color;
	Some(unsafe { self.objects.get(idx)?.assume_init_ref() })
    }

    /// Gets an object by its ID.
    pub fn get(&self, id: ObjectId<T>) -> Option<&T> {
	let idx = id.idx();
	let metadata = self.metadata.get(idx)?;
	if metadata.tag != id.tag() || metadata.color == GcColor::Tombstone {
	    return None;
	}
	Some(unsafe {self.objects.get(idx)?.assume_init_ref() })
    }

    /// Gets a mutable reference to an object by its ID.
    pub fn get_mut(&mut self, id: ObjectId<T>) -> Option<&mut T> {
	let idx = id.idx();
	let metadata = self.metadata.get(idx)?;
	if metadata.tag != id.tag() || metadata.color == GcColor::Tombstone {
	    return None;
	}

	Some(unsafe {self.objects.get_mut(idx)?.assume_init_mut()})
    }

    /// Registers a new object in the store.
    pub fn register(&mut self, object: T, color: GcColor) -> ObjectId<T> {
	match self.free_idx.pop() {
	    Some(idx) => self.register_at_idx(object, idx, color),
	    None => self.register_new(object, color),
	}
    }

    fn register_new(&mut self, object: T, color: GcColor) -> ObjectId<T> {
	let (tag, idx) = (Tag::default(), self.objects.len());
	self.objects.push(MaybeUninit::new(object));
	self.metadata.push(Metadata{ tag, color });
	ObjectId::new(tag, idx)
    }

    fn register_at_idx(&mut self, object: T, idx: usize, color: GcColor) -> ObjectId<T> {
	let metadata = self.metadata.get_mut(idx).unwrap();
	assert_eq!(metadata.color, GcColor::Tombstone);
	metadata.color = color;
	self.objects[idx] = MaybeUninit::new(object);
	ObjectId::new(metadata.tag, idx)
    }

    /// Sweeps objects of a specific color from the store.
    pub fn sweep_color(&mut self, color: GcColor) -> usize {
	let mut sweeped = 0;
	for (idx, (obj, metadata)) in self.objects.iter_mut().zip(self.metadata.iter_mut()).enumerate() {
	    if metadata.color != GcColor::Tombstone && metadata.color == color {
		*metadata = Metadata{
		    tag: metadata.tag.increment(),
		    color: GcColor::Tombstone,
		};
		unsafe { obj.assume_init_drop() };
		self.free_idx.push(idx);
		sweeped += 1;
	    }
	}
	sweeped
    }
}

impl<T> Drop for TypedObjectStore<T> {
    fn drop(&mut self) {
	for (obj, metadata) in self.objects.iter_mut().zip(self.metadata.iter_mut()) {
	    if metadata.color != GcColor::Tombstone {
		unsafe { obj.assume_init_drop() }
	    }
	}
    }
}
