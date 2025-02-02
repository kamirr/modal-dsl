use cranelift::prelude::FunctionBuilder;

use crate::parse::{
    binop::{Binop, BinopKind},
    block::Block,
    expr::Expr,
    let_::Let,
    literal::{Literal, LiteralValue},
    path::Ident,
    var::Var,
    yield_::Yield,
};

use super::{
    typed::{TypedStackSlot, TypedValue},
    varstack::VarStack,
    CompileError,
};

pub struct Recursor<'fb, 'b, 'vs> {
    builder: &'fb mut FunctionBuilder<'b>,
    stack: &'vs mut VarStack,
    retss: Option<TypedStackSlot>,
}

impl<'fb, 'b, 'vs> Recursor<'fb, 'b, 'vs> {
    pub fn new(
        builder: &'fb mut FunctionBuilder<'b>,
        stack: &'vs mut VarStack,
        retss: Option<TypedStackSlot>,
    ) -> Self {
        Recursor {
            builder,
            stack,
            retss,
        }
    }

    pub fn recurse(&mut self, expr: &Expr) -> Result<TypedValue, CompileError> {
        match expr {
            Expr::Literal(Literal {
                value: LiteralValue::Float(f),
                ..
            }) => Ok(TypedValue::float(self.builder, *f)),
            Expr::Let(Let { name, value, .. }) => {
                let tv = self.recurse(value)?;
                self.stack.set(name.name.to_string(), tv);
                Ok(tv)
            }
            Expr::Var(Var {
                name: Ident { name, span },
            }) => self.stack.get(&name).ok_or_else(|| {
                CompileError::new(format!("Variable {} not in scope", name), span.clone())
            }),
            Expr::Block(Block {
                exprs, ret_last, ..
            }) => {
                self.stack.push();
                let mut last = TypedValue::UNIT;
                for expr in exprs {
                    last = self.recurse(expr)?;
                }
                self.stack.pop();

                Ok(if *ret_last { last } else { TypedValue::UNIT })
            }
            Expr::Binop(Binop {
                left,
                right,
                op,
                span,
            }) => {
                use BinopKind::*;

                let mut l = self.recurse(left)?;
                let mut r = self.recurse(right)?;

                match op {
                    Add | Sub | Mul | Div => {
                        l = l.autoderef(self.builder);
                        r = r.autoderef(self.builder);
                    }
                    Assign => {
                        r = r.autoderef(self.builder);
                    }
                }

                match op {
                    Add => l.add(self.builder, r),
                    Sub => l.sub(self.builder, r),
                    Mul => l.mul(self.builder, r),
                    Div => l.div(self.builder, r),
                    Assign => l.assign(self.builder, r),
                }
                .map_err(|e| CompileError::new(e.msg(), span.clone()))
            }
            Expr::Yield(Yield { value, span }) => {
                let Some(retss) = self.retss else {
                    panic!("retss not provided")
                };

                let tv = self.recurse(value)?;
                tv.autoderef(self.builder)
                    .stack_store(self.builder, retss)
                    .map_err(|e| CompileError::new(e.msg(), span.clone()))
            }
            Expr::Call(_call) => todo!(),
        }
    }
}
