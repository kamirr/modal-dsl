use std::collections::HashMap;

use cranelift::prelude::{FunctionBuilder, InstBuilder};
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
    typed::{LoadCache, Ptr, TypedStackSlot, TypedValue, TypedValueImpl, ValueType},
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
                    Add | Sub | Mul | Div | Lt => {
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
                    Lt => l.lt(self.builder, r),
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
            Expr::If(if_) => {
                let cond_tv = self.recurse(&if_.cond)?;
                let cond_v = cond_tv
                    .value(self.builder)
                    .map_err(|e| CompileError::new(e.msg(), if_.span.clone()))?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder
                    .ins()
                    .brif(cond_v, then_block, &[], else_block, &[]);

                let og_cache = self.load_cache.clone();

                // then
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let (cache_then, then_v) = {
                    self.load_cache = og_cache.clone();
                    self.load_cache.enter_branch();
                    let then_v = self.recurse(&if_.then.clone().into())?;
                    self.load_cache.exit_branch();

                    (self.load_cache.clone(), then_v)
                };
                let then_ret_args = if then_v.value_type() == ValueType::Unit {
                    vec![]
                } else {
                    vec![then_v
                        .value(self.builder)
                        .map_err(|e| CompileError::new(e.msg(), if_.span.clone()))?]
                };
                self.builder.ins().jump(merge_block, &then_ret_args);

                // else
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let (cache_else, else_v) = {
                    self.load_cache = og_cache.clone();
                    self.load_cache.enter_branch();
                    let else_v = self.recurse(&if_.else_.clone().into())?;
                    self.load_cache.exit_branch();

                    (self.load_cache.clone(), else_v)
                };
                let else_ret_args = if else_v.value_type() == ValueType::Unit {
                    vec![]
                } else {
                    vec![else_v
                        .value(self.builder)
                        .map_err(|e| CompileError::new(e.msg(), if_.span.clone()))?]
                };
                self.builder.ins().jump(merge_block, &else_ret_args);

                // merge
                let then_vt = then_v.value_type();
                let else_vt = else_v.value_type();
                let ret_vt = if then_vt == else_vt {
                    then_vt
                } else {
                    return Err(CompileError { span: if_.span.clone(), msg: format!("Then branch returns {then_vt:?}, while else branch returns {else_vt:?}") });
                };

                if ret_vt != ValueType::Unit {
                    let ret_cl_t = ret_vt.cl_type().ok_or_else(|| {
                        CompileError::new(
                            format!("{ret_vt:?} doesn't have a corresponding CLIF type"),
                            if_.span.clone(),
                        )
                    })?;
                    self.builder.append_block_param(merge_block, ret_cl_t);
                }

                self.load_cache = LoadCache::intersection(cache_then, cache_else);
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);

                match self.builder.block_params(merge_block) {
                    [] => Ok(TypedValue::UNIT),
                    [v] => Ok(unsafe {
                        match ret_vt {
                            ValueType::Unit => unreachable!(),
                            ValueType::ExternPtr(et) => TypedValue::from_inner(
                                TypedValueImpl::ExternPtr(Ptr::Value(*v), et),
                            ),
                            ValueType::ExternPtrRef(et) => TypedValue::from_inner(
                                TypedValueImpl::ExternPtrRef(Ptr::Value(*v), et),
                            ),
                            ValueType::Float => TypedValue::from_inner(TypedValueImpl::Float(*v)),
                            ValueType::FloatRef => {
                                TypedValue::from_inner(TypedValueImpl::FloatRef(Ptr::Value(*v)))
                            }
                            ValueType::Bool => TypedValue::from_inner(TypedValueImpl::Bool(*v)),
                        }
                    }),
                    _ => unreachable!(),
                }
            }
        }
    }
}
