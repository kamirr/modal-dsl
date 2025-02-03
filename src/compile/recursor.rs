use std::collections::HashMap;

use cranelift::prelude::FunctionBuilder;
use cranelift_codegen::ir::FuncRef;

use crate::parse::{
    binop::{Binop, BinopKind},
    block::Block,
    expr::Expr,
    let_::Let,
    literal::{Literal, LiteralValue},
    path::Ident,
    yield_::Yield,
};

use super::{
    library::ExternFunc,
    typed::{LoadCache, TypedStackSlot, TypedValue},
    varstack::VarStack,
    CompileError,
};

pub struct Recursor<'fb, 'b, 'vs> {
    builder: &'fb mut FunctionBuilder<'b>,
    stack: &'vs mut VarStack,
    retss: Option<TypedStackSlot>,
    extern_funs: HashMap<String, (FuncRef, ExternFunc)>,
    load_cache: LoadCache,
}

impl<'fb, 'b, 'vs> Recursor<'fb, 'b, 'vs> {
    pub fn new(
        builder: &'fb mut FunctionBuilder<'b>,
        stack: &'vs mut VarStack,
        retss: Option<TypedStackSlot>,
        extern_funs: HashMap<String, (FuncRef, ExternFunc)>,
    ) -> Self {
        Recursor {
            builder,
            stack,
            retss,
            extern_funs,
            load_cache: Default::default(),
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
            Expr::Var(Ident { name, span }) => self.stack.get(&name).ok_or_else(|| {
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
                        l = self.load_cache.autoderef(self.builder, l);
                        r = self.load_cache.autoderef(self.builder, r);
                    }
                    Assign => {
                        r = self.load_cache.autoderef(self.builder, r);
                    }
                }

                match op {
                    Add => l.add(self.builder, r),
                    Sub => l.sub(self.builder, r),
                    Mul => l.mul(self.builder, r),
                    Div => l.div(self.builder, r),
                    Assign => self.load_cache.store(self.builder, l, r),
                }
                .map_err(|e| CompileError::new(e.msg(), span.clone()))
            }
            Expr::Yield(Yield { value, span }) => {
                let Some(retss) = self.retss else {
                    panic!("retss not provided")
                };

                let tv = self.recurse(value)?;
                self.load_cache
                    .autoderef(self.builder, tv)
                    .stack_store(self.builder, retss)
                    .map_err(|e| CompileError::new(e.msg(), span.clone()))
            }
            Expr::Call(call) => {
                let mut arg_tvs = Vec::new();
                for arg in &call.args {
                    let arg_tv = self.recurse(arg)?;
                    arg_tvs.push(self.load_cache.autoderef(self.builder, arg_tv));
                }

                let (func_ref, func_desc) = self.extern_funs.get(&call.path.0[0].name).unwrap();

                TypedValue::call(self.builder, *func_ref, func_desc, arg_tvs.as_slice())
                    .map_err(|e| CompileError::new(e.msg(), call.span.clone()))
            }
        }
    }
}
