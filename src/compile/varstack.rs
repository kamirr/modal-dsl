use super::typed::TypedValue;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VarStack {
    inner: Vec<HashMap<String, TypedValue>>,
}

impl VarStack {
    pub fn new() -> Self {
        VarStack {
            inner: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.inner.pop().unwrap();
    }

    pub fn get(&self, name: &str) -> Option<TypedValue> {
        for layer in self.inner.iter().rev() {
            if let Some(entry) = layer.get(name) {
                return Some(*entry);
            }
        }

        None
    }

    pub fn set(&mut self, name: String, tv: TypedValue) {
        self.inner.last_mut().unwrap().insert(name, tv);
    }
}
