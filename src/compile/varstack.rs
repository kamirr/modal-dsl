use crate::parse::state::StateVarType;

use super::{state_storage::MappedStorage, typed::TypedValue};
use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub struct VarStack {
    state_storage: Arc<MappedStorage>,
    inner: Vec<HashMap<String, TypedValue>>,
}

impl VarStack {
    pub fn new(state_storage: Arc<MappedStorage>) -> Self {
        VarStack {
            state_storage,
            inner: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.inner.pop().unwrap();
    }

    pub fn get(&self, name: &str) -> anyhow::Result<TypedValue> {
        for layer in self.inner.iter().rev() {
            if let Some(entry) = layer.get(name) {
                return Ok(*entry);
            }
        }

        if let Some((ty, ptr)) = self.state_storage.get(name) {
            return match ty {
                StateVarType::Float => Ok(unsafe { TypedValue::float_ref(ptr.as_ptr()) }),
            };
        }

        Err(anyhow::Error::msg(format!("Variable {name} undefined")))
    }

    pub fn set(&mut self, name: String, tv: TypedValue) {
        self.inner.last_mut().unwrap().insert(name, tv);
    }
}
