use std::{collections::HashMap, ops::Range};

use cranelift::prelude::{FloatCC, FunctionBuilder, InstBuilder};
use cranelift_codegen::ir::{entities, FuncRef};

use crate::parse::{
    array::Array,
    binop::{Binop, BinopKind},
    block::Block,
    call::Call,
    expr::Expr,
    if_::If,
    let_::Let,
    literal::{Literal, LiteralValue},
    loop_::{Break, Loop},
    path::{Number, Path, Segment},
    unop::{Unop, UnopKind},
    yield_::Yield,
};

use super::{
    library::ExternFunc,
    typed::{ArrayBuilder, LoadCache, TypedStackSlot, TypedValue, ValueType},
    varstack::VarStack,
    CompileError,
};

/// Instruct control flow of the recursor
///
/// Some expressions, like `break`, do not yield a value that could be handled
/// by naive recursion. Moreover, a jump always closes a Cranelift block, so we
/// can insert no more instructions.
///
/// This enum describes that a recursion can either yield a result, or that it
/// has diverged and thus we must abstain from all normal work and handle this
/// information. This is a responsibility of `recurse_EXPR` functions that
/// manipulate Cranelift blocks. All other recursions can simply bubble the
/// [`RecurseFlow::BlockDone`] result upwards.
enum RecurseFlow<T> {
    Continue(T),
    BlockDone,
}

impl<T> RecurseFlow<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> RecurseFlow<U> {
        match self {
            RecurseFlow::Continue(t) => RecurseFlow::Continue(f(t)),
            RecurseFlow::BlockDone => RecurseFlow::BlockDone,
        }
    }
}

/// State for recursively compiling the AST
pub struct Recursor<'fb, 'b, 'vs> {
    builder: &'fb mut FunctionBuilder<'b>,
    stack: &'vs mut VarStack,
    loop_stack: Vec<(entities::Block, Vec<(ValueType, Range<usize>)>)>,
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
            loop_stack: Vec::new(),
            retss,
            extern_funs,
            load_cache: Default::default(),
        }
    }

    /// Compile an [`Expr`]
    pub fn recurse(&mut self, expr: &Expr) -> Result<TypedValue, CompileError> {
        match self.recurse_i(expr)? {
            RecurseFlow::Continue(tv) => Ok(tv),
            RecurseFlow::BlockDone => todo!(),
        }
    }

    fn recurse_i(&mut self, expr: &Expr) -> Result<RecurseFlow<TypedValue>, CompileError> {
        match expr {
            Expr::Literal(literal) => self.recurse_literal(literal),
            Expr::Let(let_) => self.recurse_let(let_),
            Expr::Var(path) => self.recurse_var(path),
            Expr::Array(array) => self.recurse_array(array),
            Expr::Block(block) => self.recurse_block(block),
            Expr::Binop(binop) => self.recurse_binop(binop),
            Expr::Unop(unop) => self.recurse_unop(unop),
            Expr::Yield(yield_) => self.recurse_yield(yield_),
            Expr::Call(call) => self.recurse_call(call),
            Expr::If(if_) => self.recurse_if(if_),
            Expr::Loop(loop_) => self.recurse_loop(loop_),
            Expr::Break(break_) => self.recurse_break(break_),
        }
    }

    fn recurse_literal(
        &mut self,
        literal: &Literal,
    ) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let tv = match literal.value {
            LiteralValue::Float(f) => TypedValue::new_float(self.builder, f),
            LiteralValue::Bool(b) => TypedValue::new_bool(self.builder, b),
        };
        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_let(&mut self, let_: &Let) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let Let { name, value, .. } = let_;
        Ok(self.recurse_i(value)?.map(|tv| {
            self.stack.set(name.name.to_string(), tv.clone());
            tv
        }))
    }

    fn recurse_var(&mut self, var: &Path) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let mut tv = self.stack.get(&var.base.name).ok_or_else(|| {
            CompileError::new(
                format!("Variable {} not in scope", var.base.name),
                var.span.clone(),
            )
        })?;

        for segment in &var.tail {
            match segment {
                Segment::Member(_ident) => {
                    return Err(CompileError::new(
                        "Member access not implemented",
                        segment.span(),
                    ))
                }
                Segment::Index(Number { n, .. }) => {
                    tv = tv
                        .index(self.builder, *n)
                        .map_err(|e| CompileError::new(e.msg(), segment.span()))?;
                }
            }
        }

        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_block(&mut self, block: &Block) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let Block {
            exprs, ret_last, ..
        } = block;

        self.stack.push();
        let mut last = TypedValue::UNIT;
        for expr in exprs {
            let RecurseFlow::Continue(tv) = self.recurse_i(expr)? else {
                self.stack.pop();
                return Ok(RecurseFlow::BlockDone);
            };
            last = tv;
        }
        self.stack.pop();

        Ok(RecurseFlow::Continue(if *ret_last {
            last
        } else {
            TypedValue::UNIT
        }))
    }

    fn recurse_array(&mut self, array: &Array) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let mut array_builder = ArrayBuilder::new();
        for expr in &array.exprs {
            let RecurseFlow::Continue(tv) = self.recurse_i(expr)? else {
                return Ok(RecurseFlow::BlockDone);
            };

            array_builder
                .append(tv)
                .map_err(|e| CompileError::new(e.msg(), expr.span()))?;
        }

        let array_tv = array_builder.build();
        Ok(RecurseFlow::Continue(array_tv))
    }

    fn recurse_binop(&mut self, binop: &Binop) -> Result<RecurseFlow<TypedValue>, CompileError> {
        use BinopKind::*;

        let Binop {
            left,
            right,
            op,
            span,
        } = binop;

        let RecurseFlow::Continue(mut l) = self.recurse_i(left)? else {
            return Ok(RecurseFlow::BlockDone);
        };
        let RecurseFlow::Continue(mut r) = self.recurse_i(right)? else {
            return Ok(RecurseFlow::BlockDone);
        };

        match op {
            Add | Sub | Mul | Div | Lt | Lte | Eq | Gte | Gt => {
                l = l.autoderef(self.builder, &mut self.load_cache);
                r = r.autoderef(self.builder, &mut self.load_cache);
            }
            Assign => {
                r = r.autoderef(self.builder, &mut self.load_cache);
            }
        }

        match op {
            Add => l.add(self.builder, &r),
            Sub => l.sub(self.builder, &r),
            Mul => l.mul(self.builder, &r),
            Div => l.div(self.builder, &r),
            Lt => l.fcmp(self.builder, &r, FloatCC::LessThan),
            Lte => l.fcmp(self.builder, &r, FloatCC::LessThanOrEqual),
            Eq => l.fcmp(self.builder, &r, FloatCC::Equal),
            Gte => l.fcmp(self.builder, &r, FloatCC::GreaterThanOrEqual),
            Gt => l.fcmp(self.builder, &r, FloatCC::GreaterThan),
            Assign => self.load_cache.store(self.builder, l, r),
        }
        .map(RecurseFlow::Continue)
        .map_err(|e| CompileError::new(e.msg(), span.clone()))
    }

    fn recurse_unop(&mut self, unop: &Unop) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let RecurseFlow::Continue(arg) = self.recurse_i(&unop.expr)? else {
            return Ok(RecurseFlow::BlockDone);
        };

        let tv = match unop.op {
            UnopKind::Deref => arg.deref(self.builder, &mut self.load_cache),
            UnopKind::Neg => arg.neg(self.builder),
            UnopKind::Not => arg.not(self.builder),
        }
        .map_err(|e| CompileError::new(e.msg(), unop.span.clone()))?;

        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_yield(&mut self, yield_: &Yield) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let Yield { value, span } = yield_;

        let RecurseFlow::Continue(tv) = self.recurse_i(value)? else {
            return Ok(RecurseFlow::BlockDone);
        };

        let to_store = tv.autoderef(self.builder, &mut self.load_cache);

        let Some(retss) = &self.retss else {
            log::warn!("yield evaluated in state context (no return stack slot)");
            return Ok(RecurseFlow::Continue(tv));
        };

        retss
            .store(self.builder, &to_store)
            .map_err(|e| CompileError::new(e.msg(), span.clone()))?;

        Ok(RecurseFlow::Continue(to_store))
    }

    fn recurse_call(&mut self, call: &Call) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let mut arg_tvs = Vec::new();
        for arg in &call.args {
            let RecurseFlow::Continue(arg_tv) = self.recurse_i(arg)? else {
                return Ok(RecurseFlow::BlockDone);
            };
            arg_tvs.push(arg_tv.autoderef(self.builder, &mut self.load_cache));
        }

        let (func_ref, func_desc) = self.extern_funs.get(&call.path.base.name).unwrap();

        let tv = TypedValue::call(self.builder, *func_ref, func_desc, arg_tvs.as_slice())
            .map_err(|e| CompileError::new(e.msg(), call.span.clone()))?;

        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_if(&mut self, if_: &If) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let RecurseFlow::Continue(cond_tv) = self.recurse_i(&if_.cond)? else {
            return Ok(RecurseFlow::BlockDone);
        };

        let cond_tv = cond_tv.autoderef(self.builder, &mut self.load_cache);

        if cond_tv.value_type() != ValueType::Bool {
            return Err(CompileError {
                span: if_.cond.span(),
                msg: format!("If expected Bool, found {}", cond_tv.value_type().pretty()),
            });
        }

        let cond_v = cond_tv.value(self.builder);

        let merge_block = self.builder.create_block();
        let then_block = self.builder.create_block();
        let else_block = if_
            .else_
            .is_some()
            .then(|| self.builder.create_block())
            .unwrap_or(merge_block);

        self.builder
            .ins()
            .brif(cond_v, then_block, &[], else_block, &[]);

        let og_cache = self.load_cache.clone();

        // then
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_rec_flw = 'then_blk: {
            let (cache_then, then_v) = {
                self.load_cache = og_cache.clone();
                self.load_cache.enter_branch();
                let RecurseFlow::Continue(then_v) = self.recurse_block(&if_.then)? else {
                    break 'then_blk RecurseFlow::BlockDone;
                };
                self.load_cache.exit_branch();

                (self.load_cache.clone(), then_v)
            };
            let then_ret_args = if then_v.value_type() == ValueType::Unit {
                vec![]
            } else {
                vec![then_v.value(self.builder)]
            };
            self.builder.ins().jump(merge_block, &then_ret_args);

            RecurseFlow::Continue((then_v.value_type(), cache_then))
        };

        // else
        let else_rec_flw = if let Some(else_) = &if_.else_ {
            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let else_rec_flw = 'else_blk: {
                let (cache_else, else_v) = {
                    self.load_cache = og_cache.clone();
                    self.load_cache.enter_branch();
                    let RecurseFlow::Continue(else_v) = self.recurse_i(else_)? else {
                        break 'else_blk RecurseFlow::BlockDone;
                    };
                    self.load_cache.exit_branch();

                    (self.load_cache.clone(), else_v)
                };
                let else_ret_args = if else_v.value_type() == ValueType::Unit {
                    vec![]
                } else {
                    vec![else_v.value(self.builder)]
                };
                self.builder.ins().jump(merge_block, &else_ret_args);

                RecurseFlow::Continue((else_v.value_type(), cache_else))
            };

            else_rec_flw
        } else {
            RecurseFlow::Continue((ValueType::Unit, og_cache.clone()))
        };

        // merge
        let ret_vt = match (&then_rec_flw, &else_rec_flw) {
            (RecurseFlow::Continue((then_vt, _)), RecurseFlow::Continue((else_vt, _))) => {
                if then_vt == else_vt {
                    then_vt
                } else {
                    return Err(CompileError { span: if_.span.clone(), msg: format!("Then branch returns {then_vt:?}, while else branch returns {else_vt:?}") });
                }
            }
            (RecurseFlow::Continue((then_vt, _)), RecurseFlow::BlockDone) => then_vt,
            (RecurseFlow::BlockDone, RecurseFlow::Continue((else_vt, _))) => else_vt,
            (RecurseFlow::BlockDone, RecurseFlow::BlockDone) => {
                return Ok(RecurseFlow::BlockDone);
            }
        }.clone();

        if ret_vt != ValueType::Unit {
            let &[ret_cl_t] = ret_vt.cl_decompose().as_slice() else {
                unimplemented!()
            };
            self.builder.append_block_param(merge_block, ret_cl_t);
        }

        let xtract_cache = |(_, cache)| cache;
        self.load_cache = match (
            then_rec_flw.map(xtract_cache),
            else_rec_flw.map(xtract_cache),
        ) {
            (RecurseFlow::Continue(cache_then), RecurseFlow::Continue(cache_else)) => {
                LoadCache::intersection(cache_then, cache_else)
            }
            (RecurseFlow::Continue(cache_then), RecurseFlow::BlockDone) => cache_then,
            (RecurseFlow::BlockDone, RecurseFlow::Continue(cache_else)) => cache_else,
            (RecurseFlow::BlockDone, RecurseFlow::BlockDone) => unreachable!(),
        };
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let tv = match self.builder.block_params(merge_block) {
            [] => TypedValue::UNIT,
            [v] => unsafe { TypedValue::with_ty_value(ret_vt, *v) },
            _ => unreachable!(),
        };

        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_loop(&mut self, loop_: &Loop) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let loop_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.ins().jump(loop_block, &[]);

        self.loop_stack.push((merge_block, Vec::new()));

        self.builder.switch_to_block(loop_block);

        // If the loop has a condition, insert it at the top of the body
        if let Some(cond) = loop_.cond.clone() {
            let span = cond.span();
            self.recurse_if(&If {
                cond,
                then: Block::empty(span.clone()),
                else_: Some(Box::new(Expr::Break(Break {
                    expr: None,
                    span: span.clone(),
                }))),
                span,
            })?;
        }

        'loop_blk: {
            if let RecurseFlow::BlockDone = self.recurse_block(&loop_.body)? {
                break 'loop_blk;
            }
            self.builder.ins().jump(loop_block, &[]);
        }

        let break_vts = self.loop_stack.pop().unwrap().1;

        let loop_vt = match break_vts.as_slice() {
            [] => ValueType::Unit,
            [(fst, _)] => fst.clone(),
            [(fst, _), tail @ ..] => {
                for (el, span) in tail {
                    if el != fst {
                        return Err(CompileError {
                            span: span.clone(),
                            msg: format!("Break with type {el:?}, expected {fst:?}"),
                        });
                    }
                }

                fst.clone()
            }
        };

        if loop_vt != ValueType::Unit {
            let &[merge_block_param_ty] = loop_vt.cl_decompose().as_slice() else {
                unimplemented!()
            };

            self.builder
                .append_block_param(merge_block, merge_block_param_ty);
        }

        self.builder.seal_block(loop_block);
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let tv = match self.builder.block_params(merge_block) {
            [] => TypedValue::UNIT,
            [v] => unsafe { TypedValue::with_ty_value(loop_vt, *v) },
            _ => unreachable!(),
        };

        Ok(RecurseFlow::Continue(tv))
    }

    fn recurse_break(&mut self, break_: &Break) -> Result<RecurseFlow<TypedValue>, CompileError> {
        let ret_tv = if let Some(expr) = &break_.expr {
            let RecurseFlow::Continue(tv) = self.recurse_i(&expr)? else {
                return Ok(RecurseFlow::BlockDone);
            };
            tv
        } else {
            TypedValue::UNIT
        };

        let (merge_block, ret_records) = self.loop_stack.last_mut().unwrap();

        ret_records.push((ret_tv.value_type(), break_.span.clone()));

        let jmp_args = if ret_tv.value_type() == ValueType::Unit {
            vec![]
        } else {
            vec![ret_tv.value(self.builder)]
        };

        self.builder.ins().jump(*merge_block, &jmp_args);
        Ok(RecurseFlow::BlockDone)
    }
}
