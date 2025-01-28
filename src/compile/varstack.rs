use cranelift::prelude::Value;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct VarStack {
    inner: Vec<HashMap<String, Value>>,
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

    pub fn get(&self, name: &str) -> anyhow::Result<Value> {
        for layer in self.inner.iter().rev() {
            if let Some(entry) = layer.get(name) {
                return Ok(*entry);
            }
        }

        Err(anyhow::Error::msg(format!("Variable {name} undefined")))
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.inner.last_mut().unwrap().insert(name, value);
    }
}
