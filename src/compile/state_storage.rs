use std::{
    alloc::{alloc, dealloc, Layout},
    collections::HashMap,
    ptr::NonNull,
};

use super::typed::TypedValue;

#[derive(Debug)]
pub struct StateStorage {
    ptr: NonNull<u8>,
    len: usize,
}

impl StateStorage {
    pub fn new(len: usize) -> StateStorage {
        if len == 0 {
            StateStorage {
                ptr: NonNull::dangling(),
                len: 0,
            }
        } else {
            let ptr = unsafe {
                let ptr = alloc(Layout::from_size_align(len, 8).unwrap());
                ptr.write_bytes(0, len);

                ptr
            };

            StateStorage {
                ptr: NonNull::new(ptr).unwrap(),
                len,
            }
        }
    }

    /// Get pointer offset into storage
    ///
    /// SAFETY:
    /// Offset must point into a valid state variable
    pub unsafe fn get(&self, offset: usize) -> NonNull<u8> {
        self.ptr.add(offset)
    }
}

impl Drop for StateStorage {
    fn drop(&mut self) {
        if self.ptr != NonNull::dangling() {
            unsafe {
                dealloc(
                    self.ptr.as_ptr(),
                    Layout::from_size_align(self.len, 8).unwrap(),
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct MappedStorage {
    mapping: HashMap<String, TypedValue>,
    // storage is here to keep it alive
    #[allow(dead_code)]
    storage: StateStorage,
}

impl MappedStorage {
    /// Construct mapped storage
    ///
    /// SAFETY:
    /// Mapping must correctly describe pointers of state variables pointing to
    /// within the storage. [`StateStorage`] must have the correct size.
    pub unsafe fn new(mapping: HashMap<String, TypedValue>, storage: StateStorage) -> Self {
        MappedStorage { mapping, storage }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, TypedValue)> {
        self.mapping.iter().map(|(s, tv)| (s.as_str(), *tv))
    }
}
