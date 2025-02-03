use std::collections::HashMap;

use super::typed::ValueType;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExternType(u32);

#[derive(Debug, Default)]
pub struct ExternTypeReg {
    types: HashMap<String, ExternType>,
    cnt: u32,
}

impl ExternTypeReg {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&mut self, name: &str) -> ExternType {
        *self.types.entry(name.to_string()).or_insert_with(|| {
            self.cnt += 1;
            ExternType(self.cnt)
        })
    }
}

#[derive(Clone, Debug)]
pub struct ExternFunc {
    pub args: Vec<ValueType>,
    pub ret: Option<ValueType>,
    pub ptr: *const u8,
}

impl ExternFunc {
    pub fn new(args: Vec<ValueType>, ret: Option<ValueType>, ptr: *const u8) -> Self {
        ExternFunc { args, ret, ptr }
    }
}

#[derive(Debug, Default)]
pub struct Library {
    pub types: ExternTypeReg,
    funcs: HashMap<String, ExternFunc>,
}

impl Library {
    pub fn new() -> Self {
        Self::default()
    }

    pub unsafe fn insert_func(&mut self, name: &str, func: ExternFunc) {
        self.funcs.insert(name.to_string(), func);
    }

    pub fn get_func(&self, name: &str) -> Option<&ExternFunc> {
        self.funcs.get(name)
    }

    pub fn funcs(&self) -> impl Iterator<Item = (&str, &ExternFunc)> {
        self.funcs.iter().map(|(name, f)| (name.as_str(), f))
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&str, *const u8)> {
        self.funcs
            .iter()
            .map(|(name, fun)| (name.as_str(), fun.ptr))
    }
}

pub mod stdlib {
    use super::*;
    use std::collections::VecDeque;

    fn math(library: &mut Library) {
        extern "C" fn sin(x: f32) -> f32 {
            x.sin()
        }

        extern "C" fn cos(x: f32) -> f32 {
            x.cos()
        }

        extern "C" fn tan(x: f32) -> f32 {
            x.tan()
        }

        extern "C" fn abs(x: f32) -> f32 {
            x.abs()
        }

        extern "C" fn sqrt(x: f32) -> f32 {
            x.sqrt()
        }

        for (name, ptr) in [
            ("sin", sin as extern "C" fn(f32) -> f32),
            ("cos", cos),
            ("tan", tan),
            ("abs", abs),
            ("sqrt", sqrt),
        ] {
            unsafe {
                library.insert_func(
                    name,
                    ExternFunc::new(
                        vec![ValueType::Float],
                        Some(ValueType::Float),
                        ptr as *const u8,
                    ),
                );
            }
        }

        extern "C" fn pow(a: f32, b: f32) -> f32 {
            a.powf(b)
        }

        for (name, ptr) in [("pow", pow as extern "C" fn(f32, f32) -> f32)] {
            unsafe {
                library.insert_func(
                    name,
                    ExternFunc::new(
                        vec![ValueType::Float, ValueType::Float],
                        Some(ValueType::Float),
                        ptr as *const u8,
                    ),
                );
            }
        }
    }

    fn buffer(library: &mut Library) {
        extern "C" fn buffer_new() -> &'static mut VecDeque<f32> {
            Box::leak(Box::new(VecDeque::new()))
        }

        extern "C" fn buffer_push(buffer: &'static mut VecDeque<f32>, v: f32) {
            buffer.push_front(v);
        }

        extern "C" fn buffer_pop(buffer: &'static mut VecDeque<f32>) -> f32 {
            buffer.pop_back().unwrap_or_default()
        }

        let buffer_ty = library.types.get("buffer");
        unsafe {
            library.insert_func(
                "buffer_new",
                ExternFunc::new(
                    vec![],
                    Some(ValueType::ExternPtr(buffer_ty)),
                    buffer_new as *const u8,
                ),
            );

            library.insert_func(
                "buffer_push",
                ExternFunc::new(
                    vec![ValueType::ExternPtr(buffer_ty), ValueType::Float],
                    None,
                    buffer_push as *const u8,
                ),
            );

            library.insert_func(
                "buffer_pop",
                ExternFunc::new(
                    vec![ValueType::ExternPtr(buffer_ty)],
                    Some(ValueType::Float),
                    buffer_pop as *const u8,
                ),
            );
        }
    }

    pub fn stdlib() -> Library {
        let mut library = Library::new();
        math(&mut library);
        buffer(&mut library);

        library
    }
}
