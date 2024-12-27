use std::rc::Rc;

pub mod compiler;
pub mod instruction;
pub mod module;
pub mod object_store;
pub mod val;
pub mod vm;

pub type SporeRc<T> = Rc<T>;
