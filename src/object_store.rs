use std::{collections::HashMap, hash::Hash, marker::PhantomData};

#[derive(Debug)]
pub struct ObjectId<T>(u32, PhantomData<T>);

impl<T> PartialEq for ObjectId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
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

#[derive(Debug)]
pub struct TypedObjectStore<T> {
    id_to_object: HashMap<ObjectId<T>, T>,
    next_id: u32,
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
    pub fn get(&self, id: ObjectId<T>) -> Option<&T> {
        self.id_to_object.get(&id)
    }

    pub fn get_mut(&mut self, id: ObjectId<T>) -> Option<&mut T> {
        self.id_to_object.get_mut(&id)
    }

    pub fn register(&mut self, obj: T) -> ObjectId<T> {
        let id = ObjectId(self.next_id, PhantomData);
        self.next_id += 1;
        self.id_to_object.insert(id, obj);
        id
    }
}
