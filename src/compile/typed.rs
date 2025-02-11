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
}

impl TypedStackSlot {
    pub fn new(builder: &mut FunctionBuilder<'_>, vt: ValueType) -> Self {
        let abi = vt.abi();
        let size = abi.size;
        let align_shift = match abi.align {
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
            size: size,
            align_shift,
        });

        TypedStackSlot { ss, vt }
    }

    pub fn load(&self, builder: &mut FunctionBuilder<'_>) -> TypedValue {
        unsafe {
            TypedValue::with_loader(self.vt.clone(), &mut |cl_type, off| {
                builder.ins().stack_load(cl_type, self.ss, off as i32)
            })
        }
    }

    pub fn store(
        &self,
        builder: &mut FunctionBuilder<'_>,
        tv: &TypedValue,
    ) -> Result<(), TypedOpError> {
        if &self.vt != &tv.value_type() {
            return Err(TypedOpError::InvalidStore {
                slot_vt: self.vt.clone(),
                found: tv.value_type(),
            });
        }

        unsafe {
            tv.store(builder, &mut |builder, v, off| {
                builder.ins().stack_store(v, self.ss, off as i32);
            });
        }

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
        idx: u32,
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
            TypedOpError::OutOfBounds { vt, idx } => {
                format!("Index {} out of bounds for {}", idx, vt.pretty())
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

        let vt = unsafe {
            TypedValue::with_loader(vt, &mut |cl_ty, off| {
                builder
                    .ins()
                    .load(cl_ty, MemFlags::trusted(), ptr_v, off as i32)
            })
        };

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
        unsafe {
            src.store(builder, &mut |builder, v, off| {
                builder
                    .ins()
                    .store(MemFlags::trusted(), v, ptr_v, off as i32);
            });
        }

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
    pub fn offset(self, builder: &mut FunctionBuilder<'_>, offset: u32) -> Self {
        if offset == 0 {
            return self;
        }

        match self {
            Ptr::Literal(ptr) => Ptr::Literal(unsafe { ptr.offset(offset as isize) }),
            Ptr::Stack(ss, pre_offset) => Ptr::Stack(ss, pre_offset + offset),
            Ptr::Value(v) => {
                let off_v = builder.ins().iconst(I64, offset as i64);
                Ptr::Value(builder.ins().iadd(v, off_v))
            }
        }
    }

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
    Seq(Vec<Self>),
    Ref(Box<Self>),
}

pub struct ValueTypeAbiEntry {
    pub cl_type: Type,
    pub offset: u32,
}

pub struct ValueTypeAbi {
    pub size: u32,
    pub align: u32,
    pub entries: Vec<ValueTypeAbiEntry>,
}

impl ValueTypeAbi {
    pub fn compute(cl_types: &[Type]) -> Self {
        let mut offset = 0;
        let mut align = 1;
        let mut entries = Vec::new();
        for &ty in cl_types {
            // Assumption that align is the same as size is overly restrictive
            let (size, align_one) = (ty.bytes(), ty.bytes());

            align = align.max(align_one);
            if offset % align_one != 0 {
                offset = offset.next_multiple_of(align_one);
            }

            entries.push(ValueTypeAbiEntry {
                cl_type: ty,
                offset,
            });

            offset += size;
        }

        ValueTypeAbi {
            size: offset,
            align,
            entries,
        }
    }

    pub fn with_offsets_aux<T>(
        it: impl Iterator<Item = (ValueType, T)>,
        mut f: impl FnMut(u32, ValueType, T),
    ) {
        let mut offset = 0;
        for (vt, t) in it {
            let abi = vt.abi();

            if offset % abi.align != 0 {
                offset = offset.next_multiple_of(abi.align);
            }

            (f)(offset, vt, t);

            offset += abi.size;
        }
    }

    pub fn with_offsets(it: impl Iterator<Item = ValueType>, mut f: impl FnMut(u32, ValueType)) {
        Self::with_offsets_aux(it.map(|vt| (vt, ())), move |off, vt, ()| (f)(off, vt));
    }

    pub fn last_offset(it: impl Iterator<Item = ValueType>) -> u32 {
        let mut offset = 0;
        Self::with_offsets(it, |vt_off, _| offset = vt_off);

        offset
    }
}

impl ValueType {
    pub fn cl_decompose(&self) -> Vec<Type> {
        match self {
            ValueType::ExternPtr(_) | ValueType::Ref(_) => vec![I64],
            ValueType::Float => vec![F32],
            ValueType::Unit | ValueType::Bool => vec![I8],
            ValueType::Seq(inner) => inner
                .into_iter()
                .map(ValueType::cl_decompose)
                .map(IntoIterator::into_iter)
                .flatten()
                .collect(),
        }
    }

    pub fn abi(&self) -> ValueTypeAbi {
        ValueTypeAbi::compute(&self.cl_decompose())
    }

    /// Pretty-print the type
    pub fn pretty(&self) -> String {
        match self {
            ValueType::Unit => String::from("Unit"),
            ValueType::ExternPtr(et) => format!("Extern<{}>", et.id()),
            ValueType::Float => String::from("Float"),
            ValueType::Bool => String::from("Bool"),
            ValueType::Seq(inner) => {
                let mut s = String::from("[");
                let mut iter = inner.iter().peekable();
                while let Some(el) = iter.next() {
                    s.push_str(&el.pretty());

                    if iter.peek().is_some() {
                        s.push_str(", ");
                    }
                }

                s.push_str("]");
                s
            }
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
    Seq(Vec<TypedValue>),
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

    pub(crate) unsafe fn with_loader(
        ty: ValueType,
        loader: &mut dyn FnMut(Type, u32) -> Value,
    ) -> Self {
        Self::with_loader_impl(ty, 0, loader)
    }

    unsafe fn with_loader_impl(
        ty: ValueType,
        offset: u32,
        loader: &mut dyn FnMut(Type, u32) -> Value,
    ) -> Self {
        let tvi = match ty {
            ValueType::Unit => TypedValueImpl::Unit,
            ValueType::ExternPtr(et) => {
                TypedValueImpl::ExternPtr(Ptr::Value(loader(I64, offset)), et)
            }
            ValueType::Ref(vt) => TypedValueImpl::Ref(Ptr::Value(loader(I64, offset)), *vt),
            ValueType::Float => TypedValueImpl::Float(loader(F32, offset)),
            ValueType::Bool => TypedValueImpl::Bool(loader(I8, offset)),
            ValueType::Seq(vts) => {
                let mut tvs = Vec::new();
                ValueTypeAbi::with_offsets(vts.into_iter(), |vt_off, vt| {
                    let tv = TypedValue::with_loader_impl(vt, offset + vt_off, loader);
                    tvs.push(tv);
                });

                TypedValueImpl::Seq(tvs)
            }
        };

        TypedValue(tvi)
    }

    pub(crate) unsafe fn store(
        &self,
        builder: &mut FunctionBuilder<'_>,
        storer: &mut dyn FnMut(&mut FunctionBuilder<'_>, Value, u32),
    ) {
        self.store_impl(0, builder, storer);
    }

    unsafe fn store_impl(
        &self,
        offset: u32,
        builder: &mut FunctionBuilder<'_>,
        storer: &mut dyn FnMut(&mut FunctionBuilder<'_>, Value, u32),
    ) {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Unit => {}
            TypedValueImpl::ExternPtr(ptr, _) | TypedValueImpl::Ref(ptr, _) => {
                let v = ptr.value(builder);
                storer(builder, v, offset);
            }
            TypedValueImpl::Float(v) | TypedValueImpl::Bool(v) => storer(builder, *v, offset),
            TypedValueImpl::Seq(tvs) => {
                ValueTypeAbi::with_offsets_aux(
                    tvs.iter().map(|tv| (tv.value_type(), tv)),
                    |tv_off, _vt, tv| {
                        tv.store_impl(offset + tv_off, builder, storer);
                    },
                );
            }
        };
    }

    pub(crate) unsafe fn with_ty_value(ty: ValueType, v: Value) -> Self {
        let tvi = match ty {
            ValueType::Unit => TypedValueImpl::Unit,
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(Ptr::Value(v), et),
            ValueType::Float => TypedValueImpl::Float(v),
            ValueType::Bool => TypedValueImpl::Bool(v),
            ValueType::Ref(vt) => TypedValueImpl::Ref(Ptr::Value(v), *vt),
            ValueType::Seq(_) => panic!("can't create seq from 1 value"),
        };

        TypedValue(tvi)
    }

    pub(crate) unsafe fn with_ty_ptr(ty: ValueType, ptr: Ptr) -> Self {
        let tvi = match ty.clone() {
            ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(ptr, et),
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
        idx: u32,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(tvi) = self;

        match tvi {
            TypedValueImpl::Seq(els) => {
                if idx >= els.len() as u32 {
                    Err(TypedOpError::OutOfBounds {
                        vt: self.value_type(),
                        idx,
                    })
                } else {
                    Ok(els[idx as usize].clone())
                }
            }
            TypedValueImpl::Ref(ptr, ValueType::Seq(vts)) => {
                if idx >= vts.len() as u32 {
                    return Err(TypedOpError::OutOfBounds {
                        vt: self.value_type(),
                        idx,
                    });
                };

                let offset = ValueTypeAbi::last_offset(vts.iter().cloned().take(idx as usize + 1));
                log::debug!("INDEX {vts:?} [{idx}] => {offset}");

                Ok(TypedValue(TypedValueImpl::Ref(
                    ptr.offset(builder, offset),
                    vts[idx as usize].clone(),
                )))
            }
            _ => Err(TypedOpError::InvalidIndexInto {
                found: self.value_type(),
            }),
        }
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
            TypedValueImpl::Seq(inner) => {
                ValueType::Seq(inner.iter().map(Self::value_type).collect())
            }
            TypedValueImpl::Ref(_, vt) => ValueType::Ref(Box::new(vt.clone())),
        }
    }

    pub fn value(&self, builder: &mut FunctionBuilder<'_>) -> Value {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(v) | TypedValueImpl::Bool(v) => *v,
            TypedValueImpl::ExternPtr(ptr, _) | TypedValueImpl::Ref(ptr, _) => ptr.value(builder),
            TypedValueImpl::Unit => builder.ins().iconst(I8, 0),
            TypedValueImpl::Seq(_) => panic!(),
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

    pub fn build(self) -> TypedValue {
        TypedValue(TypedValueImpl::Seq(self.els))
    }
}
