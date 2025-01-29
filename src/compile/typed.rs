use cranelift::prelude::{
    types::F32, FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind, Value,
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

#[derive(Clone, Copy, Debug)]
enum TypedValueImpl {
    Unit,
    Float(Value),
}

#[derive(Debug, Clone, Copy)]
pub struct TypedValue(TypedValueImpl);

impl TypedValue {
    pub const UNIT: Self = TypedValue(TypedValueImpl::Unit);

    pub fn float(builder: &mut FunctionBuilder<'_>, f: f32) -> TypedValue {
        TypedValue(TypedValueImpl::Float(builder.ins().f32const(f)))
    }

    pub fn stack_load(builder: &mut FunctionBuilder<'_>, tss: TypedStackSlot) -> TypedValue {
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
    ) -> anyhow::Result<TypedValue> {
        let TypedValue(tvi) = self;
        let TypedStackSlot(tssi) = tss;
        match (tvi, tssi) {
            (TypedValueImpl::Float(value), TypedStackSlotImpl::Float(ss)) => Ok({
                builder.ins().stack_store(value, ss, 0);
                TypedValue::UNIT
            }),
            _ => Err(anyhow::Error::msg(format!(
                "Can't store {self:?} in {tss:?}"
            ))),
        }
    }

    pub fn add(self, builder: &mut FunctionBuilder<'_>, other: Self) -> anyhow::Result<TypedValue> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(anyhow::Error::msg(format!(
                "Addition not defined for {self:?} {other:?}"
            )));
        };

        let v = builder.ins().fadd(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn sub(self, builder: &mut FunctionBuilder<'_>, other: Self) -> anyhow::Result<TypedValue> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(anyhow::Error::msg(format!(
                "Subtraction not defined for {self:?} {other:?}"
            )));
        };

        let v = builder.ins().fsub(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn mul(self, builder: &mut FunctionBuilder<'_>, other: Self) -> anyhow::Result<TypedValue> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(anyhow::Error::msg(format!(
                "Multiplication not defined for {self:?} {other:?}"
            )));
        };

        let v = builder.ins().fmul(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn div(self, builder: &mut FunctionBuilder<'_>, other: Self) -> anyhow::Result<TypedValue> {
        let TypedValue(l) = self;
        let TypedValue(r) = other;

        let (TypedValueImpl::Float(l), TypedValueImpl::Float(r)) = (l, r) else {
            return Err(anyhow::Error::msg(format!(
                "Division not defined for {self:?} {other:?}"
            )));
        };

        let v = builder.ins().fdiv(l, r);
        Ok(TypedValue(TypedValueImpl::Float(v)))
    }

    pub fn inner(self) -> anyhow::Result<Value> {
        let TypedValue(tvi) = self;
        match tvi {
            TypedValueImpl::Float(v) => Ok(v),
            TypedValueImpl::Unit => Err(anyhow::Error::msg(format!(
                "{self:?} doesn't have a corresponding cranelift value"
            ))),
        }
    }
}
