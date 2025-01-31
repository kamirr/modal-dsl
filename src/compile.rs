mod recursor;
mod storage;
mod typed;
mod varstack;

use std::{collections::HashMap, sync::Arc};

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        types::F32, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
    },
};
use cranelift_codegen::{settings, Context};
use recursor::Recursor;
use storage::{MappedStorage, StorageBuf};
use typed::{TypedStackSlot, TypedValue};
use varstack::VarStack;

use crate::parse::{inputs::InputEntry, literal::Literal, state::StateEntry, Program};

pub struct CompiledProgram {
    #[allow(dead_code)]
    pub state: Arc<MappedStorage>,
    pub inputs: HashMap<String, *mut f32>,
    init: fn(),
    step: fn() -> f32,
}

impl CompiledProgram {
    pub fn init(&self) {
        (self.init)()
    }

    pub fn step(&self) -> f32 {
        (self.step)()
    }

    pub fn set_input(&self, name: &str, value: f32) {
        let ptr = self.inputs[name];
        unsafe {
            ptr.write(value);
        }
    }
}

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

    pub fn compile(&mut self, program: &Program) -> anyhow::Result<CompiledProgram> {
        let (init, state_storage, inputs) = self.build_init(program)?;

        let state_storage = Arc::new(state_storage);
        let step = self.build_step(program, Arc::clone(&state_storage))?;

        Ok(CompiledProgram {
            state: state_storage,
            inputs,
            init,
            step,
        })
    }

    fn build_init(
        &mut self,
        program: &Program,
    ) -> anyhow::Result<(fn(), MappedStorage, HashMap<String, *mut f32>)> {
        self.module_ctx.func.signature.params = vec![];
        self.module_ctx.func.signature.returns = vec![];

        let id = self.module.declare_function(
            "jit_init",
            Linkage::Export,
            &self.module_ctx.func.signature,
        )?;

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.module_ctx.func, &mut builder_ctx);

        let block = builder.create_block();
        builder.seal_block(block);

        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let mut state_tvs = Vec::<(String, TypedValue, StateKind)>::new();
        for StateEntry { name, init } in &program.state().0 {
            let mut stack = VarStack::new();
            let mut recursor = Recursor::new(&mut builder, &mut stack, None);

            let init_v = recursor.recurse(init);
            state_tvs.push((name.0.clone(), init_v, StateKind::InternalState));
        }
        for InputEntry { name, default, .. } in &program.inputs().0 {
            let mut stack = VarStack::new();
            let mut recursor = Recursor::new(&mut builder, &mut stack, None);

            let init_v = recursor.recurse(&default.unwrap_or(Literal::Float(0.0)).into());
            state_tvs.push((name.0.clone(), init_v, StateKind::Input));
        }
        let mapped_storage = Self::build_state_storage(&state_tvs)?;

        let mut inputs = HashMap::new();
        for (name, ptr_v) in mapped_storage.iter() {
            let (init_v, kind) = state_tvs
                .iter()
                .find_map(|(id, init_v, kind)| (id == name).then_some((*init_v, *kind)))
                .unwrap();

            if kind == StateKind::Input {
                inputs.insert(name.to_string(), ptr_v.as_ptr() as *mut f32);
            }
            ptr_v.assign(&mut builder, init_v).unwrap();
        }

        builder.ins().return_(&[]);

        println!("init:\n{}", builder.func.display());
        builder.finalize();

        self.module.define_function(id, &mut self.module_ctx)?;

        self.module.clear_context(&mut self.module_ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        let func = unsafe { std::mem::transmute::<*const u8, fn()>(code) };

        Ok((func, mapped_storage, inputs))
    }

    fn build_step(
        &mut self,
        program: &Program,
        mapped_storage: Arc<MappedStorage>,
    ) -> anyhow::Result<fn() -> f32> {
        self.module_ctx.func.signature.params = vec![];
        self.module_ctx.func.signature.returns = vec![AbiParam::new(F32)];

        let id = self.module.declare_function(
            "jit_step",
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
        for (name, ref_tv) in mapped_storage.iter() {
            stack.set(name.to_string(), ref_tv);
        }

        let retss = TypedStackSlot::float()(&mut builder);

        let mut recursor = Recursor::new(&mut builder, &mut stack, Some(retss));
        for expr in &program.step().0.exprs {
            recursor.recurse(expr);
        }

        let read_ret = TypedValue::stack_load(&mut builder, retss).inner().unwrap();
        builder.ins().return_(&[read_ret]);

        println!("step:\n{}", builder.func.display());
        builder.finalize();

        self.module.define_function(id, &mut self.module_ctx)?;

        self.module.clear_context(&mut self.module_ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        let func = unsafe { std::mem::transmute::<*const u8, fn() -> f32>(code) };

        Ok(func)
    }

    fn build_state_storage(
        entries: &[(String, TypedValue, StateKind)],
    ) -> anyhow::Result<MappedStorage> {
        let mut offset = 0usize;
        let mut state_mapping = HashMap::new();
        let mut offsets = HashMap::new();

        for (name, tv, _kind) in entries {
            let abi_type = tv.cl_type()?;

            // Assumption that align is the same as size is overly restrictive
            let (size, align) = (abi_type.bytes() as usize, abi_type.bytes() as usize);

            if offset % align != 0 {
                offset = offset.next_multiple_of(align);
            }

            offsets.insert(name.clone(), offset);

            offset += size;
        }

        let storage_len = offset;
        let storage = StorageBuf::new(storage_len);

        for (name, tv, _kind) in entries {
            let offset = offsets.get(name).unwrap();
            state_mapping.insert(name.clone(), unsafe {
                tv.ref_this(storage.get(*offset).as_ptr())
            });
        }

        // See MappedStorage::new invariants
        Ok(unsafe { MappedStorage::new(state_mapping, storage) })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StateKind {
    InternalState,
    Input,
}
