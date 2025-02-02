use std::{
    alloc::{alloc, dealloc, Layout},
    collections::HashMap,
    ptr::NonNull,
};

use cranelift::prelude::{types, Type};

use super::typed::{TypedValue, TypedValueImpl};

#[derive(Debug)]
pub struct StorageBuf {
    ptr: NonNull<u8>,
    len: usize,
}

impl StorageBuf {
    pub fn new(len: usize) -> StorageBuf {
        if len == 0 {
            StorageBuf {
                ptr: NonNull::dangling(),
                len: 0,
            }
        } else {
            let ptr = unsafe {
                let ptr = alloc(Layout::from_size_align(len, 8).unwrap());
                ptr.write_bytes(0, len);

                ptr
            };

            StorageBuf {
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

impl Drop for StorageBuf {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageEntryKind {
    Internal,
    External,
}

#[derive(Debug, Clone, Copy)]
pub struct StorageEntry {
    pub ty: Type,
    pub kind: StorageEntryKind,
    pub ptr: *mut u8,
}

#[derive(Debug)]
pub struct MappedStorage {
    mapping: HashMap<String, StorageEntry>,
    // storage is here to keep it alive
    #[allow(dead_code)]
    storage: StorageBuf,
}

impl MappedStorage {
    /// Construct mapped storage
    ///
    /// SAFETY:
    /// Mapping must correctly describe pointers of state variables pointing to
    /// within the storage. [`StateStorage`] must have the correct size.
    pub unsafe fn new(mapping: HashMap<String, StorageEntry>, storage: StorageBuf) -> Self {
        MappedStorage { mapping, storage }
    }

    pub fn get(&self, name: &str) -> Option<StorageEntry> {
        self.mapping.get(name).copied()
    }

    pub fn typed_values(&self) -> impl Iterator<Item = (&str, TypedValue)> {
        self.mapping.iter().map(|(s, entry)| {
            (s.as_str(), unsafe {
                assert_eq!(entry.ty, types::F32);
                TypedValue::from_inner(TypedValueImpl::FloatRef(entry.ptr as *mut f32))
            })
        })
    }
}
