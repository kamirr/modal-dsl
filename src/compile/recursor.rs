use cranelift::prelude::FunctionBuilder;

use crate::parse::{
    binop::{Binop, BinopKind},
    block::Block,
    expr::Expr,
    let_::Let,
    literal::Literal,
    yield_::Yield,
};

use super::{
    typed::{TypedStackSlot, TypedValue},
    varstack::VarStack,
};

pub struct Recursor<'fb, 'b, 'vs> {
    builder: &'fb mut FunctionBuilder<'b>,
    stack: &'vs mut VarStack,
    retss: TypedStackSlot,
}

impl<'fb, 'b, 'vs> Recursor<'fb, 'b, 'vs> {
    pub fn new(
        builder: &'fb mut FunctionBuilder<'b>,
        stack: &'vs mut VarStack,
        retss: TypedStackSlot,
    ) -> Self {
        Recursor {
            builder,
            stack,
            retss,
        }
    }

    pub fn recurse(&mut self, expr: &mut Expr) -> TypedValue {
        match expr {
            Expr::Literal(Literal::Float(f)) => TypedValue::float(self.builder, *f),
            Expr::Let(Let { name, value }) => {
                let tv = self.recurse(value);
                self.stack.set(name.0.to_string(), tv);
                tv
            }
            Expr::Var(var) => self.stack.get(&var.name.0).unwrap(),
            Expr::Block(Block { exprs, ret_last }) => {
                self.stack.push();
                let last = exprs
                    .iter_mut()
                    .map(|expr| self.recurse(expr))
                    .last()
                    .unwrap_or(TypedValue::UNIT);
                self.stack.pop();

                if *ret_last {
                    last
                } else {
                    TypedValue::UNIT
                }
            }
            Expr::Binop(Binop { left, right, op }) => {
                let l = self.recurse(left);
                let r = self.recurse(right);

                match op {
                    BinopKind::Add => l.add(self.builder, r),
                    BinopKind::Sub => l.sub(self.builder, r),
                    BinopKind::Mul => l.mul(self.builder, r),
                    BinopKind::Div => l.div(self.builder, r),
                }
                .unwrap()
            }
            Expr::Yield(Yield { value }) => {
                let tv = self.recurse(value);
                tv.stack_store(self.builder, self.retss).unwrap()
            }
            _ => todo!(),
        }
    }
}
