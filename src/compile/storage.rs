use std::{
    alloc::{alloc, dealloc, Layout},
    collections::HashMap,
    ptr::NonNull,
};

use super::typed::{Ptr, TypedValue, ValueType};

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
            let layout = Layout::from_size_align(len, 8).unwrap();
            // SAFETY:
            // - layout has non-zero size.
            // - returned ptr is asserted to be non-zero.
            //
            // Both are guaranteed within this function.
            let ptr = unsafe {
                let ptr = alloc(layout);
                assert!(!ptr.is_null());
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
            // SAFETY:
            // - layout is the same as during allocation
            // - pointer is allocated by the global allocator
            //
            // Layout is copy-paste from StorageBuf::new and the pointer comes
            // directly from alloc call in the constructor.
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

#[derive(Debug, Clone)]
pub struct StorageEntry {
    pub abi: ValueType,
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
    /// within the storage and their types. [`StateStorage`] must have the correct
    /// size.
    ///
    /// Mismatched [`StorageEntryKind`] does not result in UB.
    pub unsafe fn new(mapping: HashMap<String, StorageEntry>, storage: StorageBuf) -> Self {
        MappedStorage { mapping, storage }
    }

    pub fn get(&self, name: &str) -> Option<StorageEntry> {
        self.mapping.get(name).cloned()
    }

    pub fn typed_values(&self) -> impl Iterator<Item = (&str, TypedValue)> {
        self.mapping.iter().map(|(s, entry)| {
            (s.as_str(), {
                // SAFETY
                // - ptr points to a valid storage slot.
                //
                // This is guaranteed by `MappedStorage::new` invariants.
                unsafe {
                    TypedValue::with_ty_ptr(
                        ValueType::Ref(Box::new(entry.abi.clone())),
                        Ptr::Literal(entry.ptr),
                    )
                }
            })
        })
    }
}
