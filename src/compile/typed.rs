use cranelift::prelude::{
    types::{F32, I64},
    FunctionBuilder, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_codegen::ir::StackSlot;

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
            TypedOpError::ClType { this } => format!("{this:?} does not have a cranelift type"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum TypedValueImpl {
    Unit,
    Float(Value),
    FloatRef(*mut f32),
}

#[derive(Debug, Clone, Copy)]
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
            TypedValueImpl::FloatRef(ptr) => ptr as *mut u8,
            TypedValueImpl::Float(_) | TypedValueImpl::Unit => panic!(),
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

    pub fn assign(
        self,
        builder: &mut FunctionBuilder<'_>,
        other: Self,
    ) -> Result<TypedValue, TypedOpError> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::FloatRef(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(TypedOpError::InvalidOp {
                lhs: self,
                rhs: other,
                op: "=",
            });
        };

        let ptr = builder.ins().iconst(I64, l as i64);
        builder.ins().store(MemFlags::trusted(), r, ptr, 0);

        Ok(other)
    }

    pub fn autoderef(self, builder: &mut FunctionBuilder<'_>) -> Self {
        let TypedValue(tvi) = self;
        TypedValue(match tvi {
            TypedValueImpl::Float(v) => TypedValueImpl::Float(v),
            TypedValueImpl::FloatRef(ptr) => {
                let ptr = builder.ins().iconst(I64, ptr as i64);
                TypedValueImpl::Float(builder.ins().load(F32, MemFlags::trusted(), ptr, 0))
            }
            TypedValueImpl::Unit => TypedValueImpl::Unit,
        })
    }

    pub fn value(self) -> Result<Value, TypedOpError> {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(v) => Ok(v),
            TypedValueImpl::FloatRef(_) | TypedValueImpl::Unit => {
                Err(TypedOpError::ClValue { this: self })
            }
        }
    }

    pub fn cl_type(self) -> Result<Type, TypedOpError> {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(_) => Ok(F32),
            TypedValueImpl::FloatRef(_) | TypedValueImpl::Unit => {
                Err(TypedOpError::ClType { this: self })
            }
        }
    }
}
