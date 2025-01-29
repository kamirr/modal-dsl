mod varstack;

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        types::F32, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        StackSlotData, StackSlotKind,
    },
};
use cranelift_codegen::{ir::StackSlot, settings, Context};
use varstack::{TypedValue, VarStack};

use crate::parse::{
    binop::BinopKind, block::Block, expr::Expr, let_::Let, literal::Literal, yield_::Yield, Program,
};

pub struct Compiler {
    module: JITModule,
    module_ctx: Context,
}

impl Compiler {
    pub fn new() -> anyhow::Result<Self> {
        let flags = [
            ("use_colocated_libcalls", "false"),
            ("is_pic", "false"),
            ("opt_level", "speed"),
            ("enable_alias_analysis", "true"),
        ];

        let mut flag_builder = settings::builder();
        for (flag, value) in flags {
            flag_builder.set(flag, value)?;
        }

        let isa_builder = cranelift_native::builder().map_err(anyhow::Error::msg)?;

        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
        let builder = JITBuilder::with_isa(isa, default_libcall_names());

        let module = JITModule::new(builder);
        let module_ctx = module.make_context();

        Ok(Compiler { module, module_ctx })
    }

    pub fn compile(&mut self, program: &mut Program) -> anyhow::Result<fn() -> f32> {
        self.module_ctx.func.signature.params = vec![];
        self.module_ctx.func.signature.returns = vec![AbiParam::new(F32)];

        let id = self.module.declare_function(
            "jit_main",
            Linkage::Export,
            &self.module_ctx.func.signature,
        )?;

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.module_ctx.func, &mut builder_ctx);

        let block = builder.create_block();
        builder.seal_block(block);

        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let mut stack = VarStack::new();
        let retss =
            builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 4, 4));

        for expr in &mut program.step_mut().0.exprs {
            Self::recurse(&mut builder, retss, &mut stack, expr);
        }

        let read_ret = builder.ins().stack_load(F32, retss, 0);
        builder.ins().return_(&[read_ret]);

        println!("{}", builder.func.display());
        builder.finalize();

        self.module.define_function(id, &mut self.module_ctx)?;

        self.module.clear_context(&mut self.module_ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        let func = unsafe { std::mem::transmute::<*const u8, fn() -> f32>(code) };

        Ok(func)
    }

    fn recurse(
        builder: &mut FunctionBuilder<'_>,
        retss: StackSlot,
        stack: &mut VarStack,
        expr: &mut Expr,
    ) -> TypedValue {
        match expr {
            Expr::Literal(Literal::Float(f)) => TypedValue::Float(builder.ins().f32const(*f)),
            Expr::Let(Let { name, value }) => {
                let tv = Self::recurse(builder, retss, stack, value);
                stack.set(name.0.to_string(), tv);
                tv
            }
            Expr::Var(var) => {
                let tv = stack.get(&var.name.0).unwrap();

                tv
            }
            Expr::Block(Block { exprs, ret_last }) => {
                stack.push();
                let mut last = None;
                for expr in exprs {
                    last = Some(Self::recurse(builder, retss, stack, expr));
                }
                stack.pop();
                if *ret_last {
                    last.unwrap()
                } else {
                    TypedValue::Unit
                }
            }
            Expr::Binop(binop) => {
                let l_tv = Self::recurse(builder, retss, stack, &mut binop.left);
                let r_tv = Self::recurse(builder, retss, stack, &mut binop.right);

                let TypedValue::Float(l) = l_tv else { todo!() };
                let TypedValue::Float(r) = r_tv else { todo!() };

                let val = match binop.op {
                    BinopKind::Add => builder.ins().fadd(l, r),
                    BinopKind::Sub => builder.ins().fsub(l, r),
                    BinopKind::Mul => builder.ins().fmul(l, r),
                    BinopKind::Div => builder.ins().fdiv(l, r),
                };

                TypedValue::Float(val)
            }
            Expr::Yield(Yield { value }) => {
                let tv = Self::recurse(builder, retss, stack, value);

                let TypedValue::Float(value) = tv else {
                    todo!()
                };

                builder.ins().stack_store(value, retss, 0);
                tv
            }
            _ => todo!(),
        }
    }
}
