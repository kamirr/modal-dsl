use std::collections::HashMap;

use cranelift::prelude::{
    types::{F32, I64, I8},
    FloatCC, FunctionBuilder, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_codegen::ir::{FuncRef, StackSlot};

use super::library::{ExternFunc, ExternType};

#[derive(Clone, Debug)]
pub struct TypedStackSlot {
    ss: StackSlot,
    vt: ValueType,
    el_size: u32,
    count: u32,
}

impl TypedStackSlot {
    pub fn new(builder: &mut FunctionBuilder<'_>, vt: ValueType, count: u32) -> Self {
        assert!(count >= 1, "Cannot create zero-size stack slot");

        let size = vt.cl_type().bytes();

        let align_shift = match size {
            0 => unreachable!(),
            1 => 0,     // align = 1
            2 => 1,     // align = 2
            3 | 4 => 2, // align = 4
            _ => 3,     // align = 8
        };

        log::debug!(
            "Stack slot data for {} is: size={}, align_shift={}",
            vt.pretty(),
            size,
            align_shift
        );

        let ss = builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: size * count,
            align_shift,
        });

        TypedStackSlot {
            ss,
            vt,
            el_size: size,
            count,
        }
    }

    pub fn load(&self, builder: &mut FunctionBuilder<'_>, offset: u32) -> TypedValue {
        assert!(
            offset < self.count,
            "out-of-bounds load of {self:?}, offset={offset}"
        );

        let v =
            builder
                .ins()
                .stack_load(self.vt.cl_type(), self.ss, (self.el_size * offset) as i32);
        unsafe { TypedValue::with_ty_value(self.vt.clone(), v) }
    }

    pub fn store(
        &self,
        builder: &mut FunctionBuilder<'_>,
        tv: &TypedValue,
        offset: u32,
    ) -> Result<(), TypedOpError> {
        assert!(
            offset < self.count,
            "out-of-bounds store of {tv:?} to {self:?}, offset={offset}"
        );

        if &self.vt != &tv.value_type() {
            return Err(TypedOpError::InvalidStore {
                slot_vt: self.vt.clone(),
                found: tv.value_type(),
            });
        }

        let v = tv.value(builder);
        builder
            .ins()
            .stack_store(v, self.ss, (self.el_size * offset) as i32);

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum TypedOpError {
    InvalidStore {
        slot_vt: ValueType,
        found: ValueType,
    },
    InvalidUnop {
        arg: ValueType,
        op: &'static str,
    },
    InvalidOp {
        lhs: ValueType,
        rhs: ValueType,
        op: &'static str,
    },
    InvalidArrayEl {
        deduced: ValueType,
        found: ValueType,
    },
    InvalidIndexInto {
        found: ValueType,
    },
    OutOfBounds {
        vt: ValueType,
        offset: u32,
    },
    InvalidCallArgsN {
        expected: usize,
        found: usize,
    },
    InvaldCallArgTy {
        expected: ValueType,
        found: ValueType,
        arg_n: usize,
    },
}

impl TypedOpError {
    pub fn msg(&self) -> String {
        match self {
            TypedOpError::InvalidStore { slot_vt, found } => {
                format!(
                    "Can't store {} in a stack slot of type {}",
                    found.pretty(),
                    slot_vt.pretty()
                )
            }
            TypedOpError::InvalidUnop { arg, op } => {
                format!("Operation {}{} undefined", op, arg.pretty())
            }
            TypedOpError::InvalidOp { lhs, rhs, op } => {
                format!(
                    "Operation {} {} {} undefined",
                    lhs.pretty(),
                    op,
                    rhs.pretty()
                )
            }
            TypedOpError::InvalidArrayEl { deduced, found } => {
                format!(
                    "Array deduced to store {}, found {}",
                    deduced.pretty(),
                    found.pretty()
                )
            }
            TypedOpError::InvalidIndexInto { found } => {
                format!("Cannot index into type {}, expected [_; N]", found.pretty())
            }
            TypedOpError::OutOfBounds { vt, offset } => {
                format!("Index {} out of bounds for {}", offset, vt.pretty())
            }
            TypedOpError::InvalidCallArgsN { expected, found } => {
                format!("Expected {expected} arguments, found {found}")
            }
            TypedOpError::InvaldCallArgTy {
                expected,
                found,
                arg_n,
            } => {
                format!(
                    "Argument #{} expected type {}, found {}",
                    arg_n,
                    expected.pretty(),
                    found.pretty()
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadCache {
    layers: Vec<HashMap<Ptr, TypedValue>>,
}

impl Default for LoadCache {
    fn default() -> Self {
        LoadCache {
            layers: vec![HashMap::default()],
        }
    }
}

impl LoadCache {
    pub fn intersection(a: LoadCache, b: LoadCache) -> LoadCache {
        let layers_cnt = a.layers.len().max(b.layers.len());
        let mut layers = vec![];
        for k in 0..layers_cnt {
            let left = a.layers.get(k).cloned().unwrap_or_default();
            let right = b.layers.get(k).cloned().unwrap_or_default();

            let mut entries = HashMap::new();
            for (k, v) in left {
                if right.contains_key(&k) {
                    entries.insert(k, v);
                }
            }

            layers.push(entries);
        }

        LoadCache { layers }
    }

    pub fn enter_branch(&mut self) {
        log::debug!("enter branch");
        self.layers.push(HashMap::default());
    }
    pub fn exit_branch(&mut self) {
        log::debug!("exit branch");
        self.layers.pop().unwrap();
    }

    pub fn deref(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        ptr: Ptr,
        vt: ValueType,
    ) -> TypedValue {
        if vt == ValueType::Unit {
            return TypedValue::UNIT;
        }

        for layer in self.layers.iter().rev() {
            if let Some(cached) = layer.get(&ptr).cloned() {
                log::debug!("re-used load: *{ptr:?} = {cached:?}");
                return cached;
            }
        }

        let ptr_v = ptr.value(builder);
        let v = builder
            .ins()
            .load(vt.cl_type(), MemFlags::trusted(), ptr_v, 0);

        let vt = unsafe { TypedValue::with_ty_value(vt.clone(), v) };

        log::debug!("cached load: *{ptr:?} = {vt:?}");
        self.layers.last_mut().unwrap().insert(ptr, vt.clone());
        vt
    }

    pub fn store(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        dst: TypedValue,
        src: TypedValue,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(dsti) = &dst;

        let TypedValueImpl::Ref(dst_ptr, dst_vt) = &dsti else {
            return Err(TypedOpError::InvalidOp {
                lhs: dst.value_type(),
                rhs: src.value_type(),
                op: "=",
            });
        };

        if dst_vt != &src.value_type() {
            return Err(TypedOpError::InvalidOp {
                lhs: dst.value_type(),
                rhs: src.value_type(),
                op: "=",
            });
        }

        // Stores to Unit are a no-op
        if dst_vt == &ValueType::Unit {
            return Ok(TypedValue::UNIT);
        }

        let ptr_v = dst_ptr.value(builder);
        let src_v = src.value(builder);
        builder.ins().store(MemFlags::trusted(), src_v, ptr_v, 0);

        // Runtime pointers may alias with anything and so must invalidate the
        // entire cache.
        if matches!(dst_ptr, Ptr::Value(_)) {
            log::debug!("clear load cache: all");
            for layer in &mut self.layers {
                layer.clear();
            }
        } else {
            log::debug!("clear load cache: {dst:?}");
            for layer in &mut self.layers {
                layer.remove(&dst_ptr);
            }
        }

        Ok(src)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Ptr {
    Literal(*mut u8),
    Stack(StackSlot, u32),
    Value(Value),
}

impl Ptr {
    pub fn value(self, builder: &mut FunctionBuilder<'_>) -> Value {
        match self {
            Ptr::Literal(iptr) => builder.ins().iconst(I64, iptr as i64),
            Ptr::Stack(ss, offset) => builder.ins().stack_addr(I64, ss, offset as i32),
            Ptr::Value(value) => value,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Unit,
    ExternPtr(ExternType),
    Float,
    Bool,
    Array(Box<Self>, u32),
    Ref(Box<Self>),
}

impl ValueType {
    /// Cranelift [`Type`] corresponding to this type
    pub fn cl_type(&self) -> Type {
        match self {
            ValueType::ExternPtr(_) | ValueType::Array(_, _) | ValueType::Ref(_) => I64,
            ValueType::Unit => I8,
            ValueType::Float => F32,
            ValueType::Bool => I8,
        }
    }

    /// Pretty-print the type
    pub fn pretty(&self) -> String {
        match self {
            ValueType::Unit => String::from("Unit"),
            ValueType::ExternPtr(et) => format!("Extern<{}>", et.id()),
            ValueType::Float => String::from("Float"),
            ValueType::Bool => String::from("Bool"),
            ValueType::Array(vt, n) => format!("[{}; {}]", vt.pretty(), n),
            ValueType::Ref(vt) => format!("&{}", vt.pretty()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TypedValueImpl {
    Unit,
    ExternPtr(Ptr, ExternType),
    Float(Value),
    Bool(Value),
    Array(Ptr, ValueType, u32),
    Ref(Ptr, ValueType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedValue(TypedValueImpl);

impl TypedValue {
    pub const UNIT: Self = TypedValue(TypedValueImpl::Unit);

    pub fn new_float(builder: &mut FunctionBuilder<'_>, f: f32) -> Self {
        TypedValue(TypedValueImpl::Float(builder.ins().f32const(f)))
    }

    pub fn new_bool(builder: &mut FunctionBuilder<'_>, b: bool) -> Self {
        TypedValue(TypedValueImpl::Bool(
            builder.ins().iconst(I8, if b { 1 } else { 0 }),
        ))
    }

    pub(crate) unsafe fn with_ty_value(ty: ValueType, v: Value) -> Self {
        let tvi = match ty {
            ValueType::Unit => TypedValueImpl::Unit,
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(Ptr::Value(v), et),
            ValueType::Float => TypedValueImpl::Float(v),
            ValueType::Bool => TypedValueImpl::Bool(v),
            ValueType::Array(vt, n) => TypedValueImpl::Array(Ptr::Value(v), *vt, n),
            ValueType::Ref(vt) => TypedValueImpl::Ref(Ptr::Value(v), *vt),
        };

        TypedValue(tvi)
    }

    pub(crate) unsafe fn with_ty_ptr(ty: ValueType, ptr: Ptr) -> Self {
        let tvi = match ty.clone() {
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(ptr, et),
            ValueType::Array(inner_vt, n) => TypedValueImpl::Array(ptr, *inner_vt, n),
            ValueType::Ref(inner_vt) => TypedValueImpl::Ref(ptr, *inner_vt),
            _ => panic!("{ty:?} is not a reference type"),
        };

        TypedValue(tvi)
    }

    pub fn deref(
        &self,
        builder: &mut FunctionBuilder<'_>,
        cache: &mut LoadCache,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(arg) = self;
        let TypedValueImpl::Ref(ptr, vt) = arg else {
            return Err(TypedOpError::InvalidUnop {
                arg: self.value_type(),
                op: "*",
            });
        };

        Ok(cache.deref(builder, *ptr, vt.clone()))
    }

    pub fn autoderef(
        &self,
        builder: &mut FunctionBuilder<'_>,
        cache: &mut LoadCache,
    ) -> TypedValue {
        let TypedValue(arg) = self;
        let TypedValueImpl::Ref(ptr, vt) = arg else {
            return self.clone();
        };

        cache.deref(builder, *ptr, vt.clone())
    }

    pub fn index(
        &self,
        builder: &mut FunctionBuilder<'_>,
        offset: u32,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(tvi) = self;
        let TypedValueImpl::Array(ptr, vt, length) = tvi else {
            return Err(TypedOpError::InvalidIndexInto {
                found: self.value_type(),
            });
        };

        if offset >= *length {
            return Err(TypedOpError::OutOfBounds {
                vt: self.value_type(),
                offset,
            });
        };

        let sz = vt.cl_type().bytes();

        let offset_ptr = match ptr {
            Ptr::Literal(ptr) => Ptr::Literal(unsafe { ptr.offset((offset * sz) as isize) }),
            Ptr::Stack(stack_slot, ex_offset) => Ptr::Stack(*stack_slot, ex_offset + (offset * sz)),
            Ptr::Value(value) => {
                let offset_v = builder.ins().iconst(I64, (offset * sz) as i64);
                Ptr::Value(builder.ins().iadd(*value, offset_v))
            }
        };

        Ok(unsafe { TypedValue::with_ty_ptr(ValueType::Ref(Box::new(vt.clone())), offset_ptr) })
    }

    pub fn neg(&self, builder: &mut FunctionBuilder<'_>) -> Result<TypedValue, TypedOpError> {
        let TypedValue(arg) = self;
        let TypedValueImpl::Float(arg) = arg else {
            return Err(TypedOpError::InvalidUnop {
                arg: self.value_type(),
                op: "-",
            });
        };

        let v = builder.ins().fneg(*arg);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn not(&self, builder: &mut FunctionBuilder<'_>) -> Result<TypedValue, TypedOpError> {
        let TypedValue(arg) = self;
        let TypedValueImpl::Bool(arg) = arg else {
            return Err(TypedOpError::InvalidUnop {
                arg: self.value_type(),
                op: "!",
            });
        };

        let v = builder.ins().bnot(*arg);
        Ok(TypedValue(TypedValueImpl::Bool(v)))
    }

    pub fn add(
        &self,
        builder: &mut FunctionBuilder<'_>,
        other: &Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self.value_type(),
                rhs: other.value_type(),
                op: "+",
            });
        };

        let v = builder.ins().fadd(*l, *r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn sub(
        &self,
        builder: &mut FunctionBuilder<'_>,
        other: &Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self.value_type(),
                rhs: other.value_type(),
                op: "-",
            });
        };

        let v = builder.ins().fsub(*l, *r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn mul(
        &self,
        builder: &mut FunctionBuilder<'_>,
        other: &Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self.value_type(),
                rhs: other.value_type(),
                op: "*",
            });
        };

        let v = builder.ins().fmul(*l, *r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn div(
        &self,
        builder: &mut FunctionBuilder<'_>,
        other: &Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self.value_type(),
                rhs: other.value_type(),
                op: "/",
            });
        };

        let v = builder.ins().fdiv(*l, *r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn fcmp(
        &self,
        builder: &mut FunctionBuilder<'_>,
        other: &Self,
        cmp: FloatCC,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self.value_type(),
                rhs: other.value_type(),
                op: "<",
            });
        };

        let v = builder.ins().fcmp(cmp, *l, *r);

        Ok(TypedValue(TypedValueImpl::Bool(v)))
    }

    pub fn call(
        builder: &mut FunctionBuilder<'_>,
        func: FuncRef,
        func_desc: &ExternFunc,
        args: &[TypedValue],
    ) -> Result<TypedValue, TypedOpError> {
        if args.len() != func_desc.args.len() {
            return Err(TypedOpError::InvalidCallArgsN {
                expected: func_desc.args.len(),
                found: args.len(),
            });
        }

        let mut arg_vs = Vec::new();
        for (n, (arg_tv, arg_abi)) in args
            .iter()
            .cloned()
            .zip(func_desc.args.iter().cloned())
            .enumerate()
        {
            let TypedValue(tvi) = arg_tv;
            let arg_v = match (tvi, arg_abi) {
                (TypedValueImpl::ExternPtr(ptr, et1), ValueType::ExternPtr(et2)) => {
                    assert_eq!(et1, et2);
                    ptr.value(builder)
                }
                (TypedValueImpl::Ref(ptr, vt), ValueType::Ref(ref_vt)) if vt == *ref_vt => {
                    ptr.value(builder)
                }
                (TypedValueImpl::Float(value), ValueType::Float) => value,
                (tvi, ty) => {
                    return Err(TypedOpError::InvaldCallArgTy {
                        expected: ty,
                        found: TypedValue(tvi).value_type(),
                        arg_n: n,
                    })
                }
            };
            arg_vs.push(arg_v);
        }

        let call = builder.ins().call(func, &arg_vs);
        match &func_desc.ret {
            None => Ok(TypedValue::UNIT),
            Some(abi) => {
                let value = builder.inst_results(call)[0];
                Ok(unsafe { TypedValue::with_ty_value(abi.clone(), value) })
            }
        }
    }

    pub fn value_type(&self) -> ValueType {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Unit => ValueType::Unit,
            TypedValueImpl::ExternPtr(_, et) => ValueType::ExternPtr(*et),
            TypedValueImpl::Float(_) => ValueType::Float,
            TypedValueImpl::Bool(_) => ValueType::Bool,
            TypedValueImpl::Array(_, vt, n) => ValueType::Array(Box::new(vt.clone()), *n),
            TypedValueImpl::Ref(_, vt) => ValueType::Ref(Box::new(vt.clone())),
        }
    }

    pub fn value(&self, builder: &mut FunctionBuilder<'_>) -> Value {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(v) | TypedValueImpl::Bool(v) => *v,
            TypedValueImpl::ExternPtr(ptr, _)
            | TypedValueImpl::Array(ptr, _, _)
            | TypedValueImpl::Ref(ptr, _) => ptr.value(builder),
            TypedValueImpl::Unit => builder.ins().iconst(I8, 0),
        }
    }
}

#[derive(Debug, Default)]
pub struct ArrayBuilder {
    vt: Option<ValueType>,
    els: Vec<TypedValue>,
}

impl ArrayBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn append(&mut self, tv: TypedValue) -> Result<(), TypedOpError> {
        if let Some(vt) = &self.vt {
            if vt != &tv.value_type() {
                return Err(TypedOpError::InvalidArrayEl {
                    deduced: vt.clone(),
                    found: tv.value_type(),
                });
            }
        }

        self.vt = Some(tv.value_type());
        self.els.push(tv);

        Ok(())
    }

    pub fn build(&mut self, builder: &mut FunctionBuilder<'_>) -> TypedValue {
        let count = self.els.len() as u32;

        let Some(vt) = self.vt.clone() else {
            let empty_array = unsafe {
                TypedValue::with_ty_ptr(
                    ValueType::Array(Box::new(ValueType::Float), 0),
                    Ptr::Literal(std::ptr::dangling_mut()),
                )
            };
            return empty_array;
        };

        // Shouldn't be reachable
        assert!(count > 0, "can't build array of size 0");

        let tss = TypedStackSlot::new(builder, vt.clone(), count);

        for (i, el_tv) in self.els.iter().enumerate() {
            tss.store(builder, el_tv, i as u32)
                .expect("array type mismatch");
        }

        unsafe {
            TypedValue::with_ty_ptr(ValueType::Array(Box::new(vt), count), Ptr::Stack(tss.ss, 0))
        }
    }
}
