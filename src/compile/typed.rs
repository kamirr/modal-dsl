use std::collections::HashMap;

use cranelift::prelude::{
    types::{F32, I64},
    FunctionBuilder, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type, Value,
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
    ClValue {
        this: TypedValue,
    },
    ClType {
        this: TypedValue,
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
            TypedOpError::ClValue { this } => {
                format!("{this:?} does not have a cranelift value")
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
            TypedOpError::ClType { this } => format!("{this:?} does not have a cranelift type"),
        }
    }
}

#[derive(Debug, Default)]
pub struct LoadCache {
    entries: HashMap<TypedValue, TypedValue>,
}

impl LoadCache {
    pub fn autoderef(&mut self, builder: &mut FunctionBuilder<'_>, tv: TypedValue) -> TypedValue {
        if let Some(&cached) = self.entries.get(&tv) {
            log::debug!("re-used load: *{tv:?} = {cached:?}");
            return cached;
        }

        let TypedValue(tvi) = tv;
        let result = TypedValue(match tvi {
            TypedValueImpl::ExternPtrRef(ptr, et) => {
                let ptr = ptr.value(builder);
                TypedValueImpl::ExternPtr(
                    Ptr::Value(builder.ins().load(I64, MemFlags::trusted(), ptr, 0)),
                    et,
                )
            }
            TypedValueImpl::FloatRef(ptr) => {
                let ptr = ptr.value(builder);
                TypedValueImpl::Float(builder.ins().load(F32, MemFlags::trusted(), ptr, 0))
            }
            other => return TypedValue(other),
        });

        log::debug!("cached load: *{tv:?} = {result:?}");
        self.entries.insert(tv, result);
        result
    }

    pub fn store(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        dst: TypedValue,
        src: TypedValue,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(dsti) = dst;
        let TypedValue(srci) = src;

        let clear_all;
        match (dsti, srci) {
            (TypedValueImpl::FloatRef(dst_ptr), TypedValueImpl::Float(src_v)) => {
                clear_all = matches!(dst_ptr, Ptr::Value(_));
                let ptr = dst_ptr.value(builder);
                builder.ins().store(MemFlags::trusted(), src_v, ptr, 0);
            }
            (TypedValueImpl::ExternPtrRef(dst_ptr, et1), TypedValueImpl::ExternPtr(src_v, et2)) => {
                clear_all = matches!(dst_ptr, Ptr::Value(_));
                assert_eq!(et1, et2);
                let ptr = dst_ptr.value(builder);
                let src_v = src_v.value(builder);
                builder.ins().store(MemFlags::trusted(), src_v, ptr, 0);
            }
            _ => {
                return Err(TypedOpError::InvalidOp {
                    lhs: dst,
                    rhs: src,
                    op: "=",
                });
            }
        }

        if clear_all {
            log::debug!("clear load cache: all");
            self.entries.clear();
        } else {
            log::debug!("clear load cache: {dst:?}");
            self.entries.remove(&dst);
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Unit,
    ExternPtr(ExternType),
    ExternPtrRef(ExternType),
    Float,
    FloatRef,
}

impl ValueType {
    pub fn cl_type(self) -> Option<Type> {
        match self {
            ValueType::ExternPtr(_) | ValueType::ExternPtrRef(_) | ValueType::FloatRef => Some(I64),
            ValueType::Float => Some(F32),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TypedValueImpl {
    Unit,
    ExternPtr(Ptr, ExternType),
    ExternPtrRef(Ptr, ExternType),
    Float(Value),
    FloatRef(Ptr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedValue(TypedValueImpl);

impl TypedValue {
    pub const UNIT: Self = TypedValue(TypedValueImpl::Unit);

    /// SAFETY:
    /// - If the TypedValueImpl contains a pointer, it must be valid in the
    ///   context of this specific program and point to the appropriate
    ///   storage entry.
    pub(crate) unsafe fn from_inner(inner: TypedValueImpl) -> Self {
        TypedValue(inner)
    }

    pub fn float(builder: &mut FunctionBuilder<'_>, f: f32) -> Self {
        TypedValue(TypedValueImpl::Float(builder.ins().f32const(f)))
    }

    pub fn as_ptr(self) -> *mut u8 {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::ExternPtr(Ptr::Literal(ptr), _) => ptr,
            TypedValueImpl::FloatRef(Ptr::Literal(ptr)) => ptr,
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
        self,
        builder: &mut FunctionBuilder<'_>,
        tss: TypedStackSlot,
    ) -> Result<Self, TypedOpError> {
        let TypedValue(tvi) = self;
        let TypedStackSlot(tssi) = tss;
        match (tvi, tssi) {
            (TypedValueImpl::Float(value), TypedStackSlotImpl::Float(ss)) => Ok({
                builder.ins().stack_store(value, ss, 0);
                TypedValue::UNIT
            }),
            _ => Err(TypedOpError::InvalidStore {
                slot: tss,
                value: self,
            }),
        }
    }

    pub fn add(
        self,
        builder: &mut FunctionBuilder<'_>,
        other: Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self,
                rhs: other,
                op: "+",
            });
        };

        let v = builder.ins().fadd(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn sub(
        self,
        builder: &mut FunctionBuilder<'_>,
        other: Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self,
                rhs: other,
                op: "-",
            });
        };

        let v = builder.ins().fsub(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn mul(
        self,
        builder: &mut FunctionBuilder<'_>,
        other: Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self,
                rhs: other,
                op: "*",
            });
        };

        let v = builder.ins().fmul(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn div(
        self,
        builder: &mut FunctionBuilder<'_>,
        other: Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self,
                rhs: other,
                op: "/",
            });
        };

        let v = builder.ins().fdiv(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
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
            .copied()
            .zip(func_desc.args.iter().copied())
            .enumerate()
        {
            let TypedValue(tvi) = arg_tv;
            let arg_v = match (tvi, arg_abi) {
                (TypedValueImpl::ExternPtr(ptr, et1), ValueType::ExternPtr(et2)) => {
                    assert_eq!(et1, et2);
                    ptr.value(builder)
                }
                (TypedValueImpl::FloatRef(ptr), ValueType::FloatRef) => ptr.value(builder),
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
        match func_desc.ret {
            None => Ok(TypedValue::UNIT),
            Some(abi) => {
                let value = builder.inst_results(call)[0];
                Ok(TypedValue(match abi {
                    ValueType::Unit => TypedValueImpl::Unit,
                    ValueType::ExternPtr(et) => TypedValueImpl::ExternPtr(Ptr::Value(value), et),
                    ValueType::ExternPtrRef(et) => {
                        TypedValueImpl::ExternPtrRef(Ptr::Value(value), et)
                    }
                    ValueType::FloatRef => TypedValueImpl::FloatRef(Ptr::Value(value)),
                    ValueType::Float => TypedValueImpl::Float(value),
                }))
            }
        }
    }

    pub fn value_type(self) -> ValueType {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Unit => ValueType::Unit,
            TypedValueImpl::ExternPtr(_, et) => ValueType::ExternPtr(et),
            TypedValueImpl::ExternPtrRef(_, et) => ValueType::ExternPtrRef(et),
            TypedValueImpl::Float(_) => ValueType::Float,
            TypedValueImpl::FloatRef(_) => ValueType::FloatRef,
        }
    }

    pub fn value(self) -> Result<Value, TypedOpError> {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(v) => Ok(v),
            TypedValueImpl::ExternPtr(Ptr::Value(v), _)
            | TypedValueImpl::ExternPtrRef(Ptr::Value(v), _)
            | TypedValueImpl::FloatRef(Ptr::Value(v)) => Ok(v),
            _ => Err(TypedOpError::ClValue { this: self }),
        }
    }
}
