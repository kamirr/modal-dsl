use std::collections::HashMap;

use cranelift::prelude::{
    types::{F32, I64, I8},
    FloatCC, FunctionBuilder, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_codegen::ir::{FuncRef, StackSlot};

use super::library::{ExternFunc, ExternType};

#[derive(Clone, Copy, Debug)]
enum TypedStackSlotImpl {
    Float(StackSlot),
}

#[derive(Clone, Copy, Debug)]
pub struct TypedStackSlot(TypedStackSlotImpl);

impl TypedStackSlot {
    pub fn float() -> impl FnOnce(&mut FunctionBuilder<'_>) -> TypedStackSlot {
        |builder: &mut FunctionBuilder<'_>| {
            TypedStackSlot(TypedStackSlotImpl::Float(builder.create_sized_stack_slot(
                StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 4,
                    align_shift: 2,
                },
            )))
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypedOpError {
    InvalidStore {
        slot: TypedStackSlot,
        value: TypedValue,
    },
    InvalidOp {
        lhs: TypedValue,
        rhs: TypedValue,
        op: &'static str,
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
            TypedOpError::InvalidStore { slot, value } => {
                format!("Can't store {value:?} in {slot:?}")
            }
            TypedOpError::InvalidOp { lhs, rhs, op } => {
                format!("Operation {lhs:?} {op} {rhs:?} undefined")
            }
            TypedOpError::InvalidCallArgsN { expected, found } => {
                format!("Expected {expected} arguments, found {found}")
            }
            TypedOpError::InvaldCallArgTy {
                expected,
                found,
                arg_n,
            } => {
                format!("Argument #{arg_n} expected type {expected:?}, found {found:?}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadCache {
    layers: Vec<HashMap<TypedValue, TypedValue>>,
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

    pub fn autoderef(&mut self, builder: &mut FunctionBuilder<'_>, tv: TypedValue) -> TypedValue {
        for layer in self.layers.iter().rev() {
            if let Some(cached) = layer.get(&tv).cloned() {
                log::debug!("re-used load: *{tv:?} = {cached:?}");
                return cached;
            }
        }

        let TypedValue(tvi) = &tv;
        let result = match tvi {
            TypedValueImpl::Ref(ptr, vt) => {
                // loads of Unit are a no-op
                if vt == &ValueType::Unit {
                    return TypedValue::UNIT;
                }

                let ptr = ptr.value(builder);
                let v = builder
                    .ins()
                    .load(vt.cl_type(), MemFlags::trusted(), ptr, 0);

                unsafe { TypedValue::with_ty_value(vt.clone(), v) }
            }
            other => return TypedValue(other.clone()),
        };

        log::debug!("cached load: *{tv:?} = {result:?}");
        self.layers.last_mut().unwrap().insert(tv, result.clone());
        result
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
                lhs: dst.clone(),
                rhs: src.clone(),
                op: "=",
            });
        };

        if dst_vt != &src.value_type() {
            return Err(TypedOpError::InvalidOp {
                lhs: dst,
                rhs: src,
                op: "=",
            });
        }

        // Stores to Unit are a no-op
        if dst_vt == &ValueType::Unit {
            return Ok(TypedValue::UNIT);
        }

        let clear_all = matches!(dst_ptr, Ptr::Value(_));

        let ptr_v = dst_ptr.value(builder);
        let src_v = src.value(builder);
        builder.ins().store(MemFlags::trusted(), src_v, ptr_v, 0);

        if clear_all {
            log::debug!("clear load cache: all");
            for layer in &mut self.layers {
                layer.clear();
            }
        } else {
            log::debug!("clear load cache: {dst:?}");
            for layer in &mut self.layers {
                layer.remove(&dst);
            }
        }

        Ok(src)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Ptr {
    Literal(*mut u8),
    Value(Value),
}

impl Ptr {
    pub(crate) fn value(self, builder: &mut FunctionBuilder<'_>) -> Value {
        match self {
            Ptr::Literal(iptr) => builder.ins().iconst(I64, iptr as i64),
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
    Ref(Box<Self>),
}

impl ValueType {
    pub fn cl_type(&self) -> Type {
        match self {
            ValueType::ExternPtr(_) | ValueType::Ref(_) => I64,
            ValueType::Unit => I8,
            ValueType::Float => F32,
            ValueType::Bool => I8,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TypedValueImpl {
    Unit,
    ExternPtr(Ptr, ExternType),
    Float(Value),
    Bool(Value),
    Ref(Ptr, ValueType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedValue(TypedValueImpl);

impl TypedValue {
    pub const UNIT: Self = TypedValue(TypedValueImpl::Unit);

    pub(crate) unsafe fn with_ty_value(ty: ValueType, v: Value) -> Self {
        let tvi = match ty {
            ValueType::Unit => TypedValueImpl::Unit,
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(Ptr::Value(v), et),
            ValueType::Float => TypedValueImpl::Float(v),
            ValueType::Bool => TypedValueImpl::Bool(v),
            ValueType::Ref(vt) => TypedValueImpl::Ref(Ptr::Value(v), (*vt).clone()),
        };

        TypedValue(tvi)
    }

    pub(crate) unsafe fn with_ty_ptr(ty: ValueType, ptr: *mut u8) -> Self {
        let tvi = match ty.clone() {
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(Ptr::Literal(ptr), et),
            ValueType::Ref(inner_vt) => TypedValueImpl::Ref(Ptr::Literal(ptr), *inner_vt),
            _ => panic!("{ty:?} is not a reference type"),
        };

        TypedValue(tvi)
    }

    pub fn float(builder: &mut FunctionBuilder<'_>, f: f32) -> Self {
        TypedValue(TypedValueImpl::Float(builder.ins().f32const(f)))
    }

    pub fn as_ptr(self) -> *mut u8 {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::ExternPtr(Ptr::Literal(ptr), _et) => ptr,
            TypedValueImpl::Ref(Ptr::Literal(ptr), _vt) => ptr,
            _ => panic!(),
        }
    }

    pub fn stack_load(builder: &mut FunctionBuilder<'_>, tss: TypedStackSlot) -> Self {
        let TypedStackSlot(tssi) = tss;
        match tssi {
            TypedStackSlotImpl::Float(ss) => {
                TypedValue(TypedValueImpl::Float(builder.ins().stack_load(F32, ss, 0)))
            }
        }
    }

    pub fn stack_store(
        &self,
        builder: &mut FunctionBuilder<'_>,
        tss: TypedStackSlot,
    ) -> Result<Self, TypedOpError> {
        let TypedValue(tvi) = self;
        let TypedStackSlot(tssi) = tss;
        match (tvi, tssi) {
            (TypedValueImpl::Float(value), TypedStackSlotImpl::Float(ss)) => Ok({
                builder.ins().stack_store(*value, ss, 0);
                TypedValue::UNIT
            }),
            _ => Err(TypedOpError::InvalidStore {
                slot: tss,
                value: self.clone(),
            }),
        }
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
                lhs: self.clone(),
                rhs: other.clone(),
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
                lhs: self.clone(),
                rhs: other.clone(),
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
                lhs: self.clone(),
                rhs: other.clone(),
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
                lhs: self.clone(),
                rhs: other.clone(),
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
                lhs: self.clone(),
                rhs: other.clone(),
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
            TypedValueImpl::Ref(_, vt) => ValueType::Ref(Box::new(vt.clone())),
        }
    }

    pub fn value(&self, builder: &mut FunctionBuilder<'_>) -> Value {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::ExternPtr(Ptr::Value(v), _)
            | TypedValueImpl::Float(v)
            | TypedValueImpl::Bool(v)
            | TypedValueImpl::Ref(Ptr::Value(v), _) => *v,
            TypedValueImpl::ExternPtr(Ptr::Literal(ptr), _)
            | TypedValueImpl::Ref(Ptr::Literal(ptr), _) => builder.ins().iconst(I64, *ptr as i64),
            TypedValueImpl::Unit => builder.ins().iconst(I8, 0),
        }
    }
}
