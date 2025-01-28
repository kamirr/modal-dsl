mod varstack;

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        types::F32, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        StackSlotData, StackSlotKind, Value,
    },
};
use cranelift_codegen::{ir::StackSlot, settings, Context};
use varstack::VarStack;

use crate::parse::{
    binop::{Binop, BinopKind},
    block::Block,
    expr::Expr,
    let_::Let,
    literal::Literal,
    path::Path,
    yield_::Yield,
    Program,
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

    pub fn compile(&mut self, program: &Program) -> anyhow::Result<fn() -> f32> {
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

        for expr in &program.step().0.exprs {
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

        let func = unsafe { std::mem::transmute::<_, fn() -> f32>(code) };

        Ok(func)
    }

    fn recurse(
        builder: &mut FunctionBuilder<'_>,
        retss: StackSlot,
        stack: &mut VarStack,
        expr: &Expr,
    ) -> Value {
        match expr {
            Expr::Literal(Literal::Float(f)) => builder.ins().f32const(*f),
            Expr::Let(Let { name, value }) => {
                let value = Self::recurse(builder, retss, stack, &value);
                stack.set(name.0.to_string(), value);
                value
            }
            Expr::Var(Path(path)) => {
                assert_eq!(path.len(), 1);
                stack.get(&path[0].0).unwrap()
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
                    Self::recurse(builder, retss, stack, &Expr::Literal(Literal::Float(0.0)))
                }
            }
            Expr::Binop(Binop { left, right, op }) => {
                let l = Self::recurse(builder, retss, stack, left);
                let r = Self::recurse(builder, retss, stack, right);
                match op {
                    BinopKind::Add => builder.ins().fadd(l, r),
                    BinopKind::Sub => builder.ins().fsub(l, r),
                    BinopKind::Mul => builder.ins().fmul(l, r),
                    BinopKind::Div => builder.ins().fdiv(l, r),
                }
            }
            Expr::Yield(Yield { value }) => {
                let value = Self::recurse(builder, retss, stack, value);
                builder.ins().stack_store(value, retss, 0);
                value
            }
            _ => todo!(),
        }
    }
}
