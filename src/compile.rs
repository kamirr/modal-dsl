mod recursor;
mod storage;
mod typed;
mod varstack;

use std::{collections::HashMap, sync::Arc};

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        types::{self, F32},
        AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
    },
};
use cranelift_codegen::{settings, Context};
use recursor::Recursor;
use storage::{MappedStorage, StorageBuf, StorageEntry, StorageEntryKind};
use typed::{TypedStackSlot, TypedValue};
use varstack::VarStack;

use crate::parse::{
    inputs::InputEntry,
    literal::{Literal, LiteralValue},
    state::StateEntry,
    Program,
};

struct InitBuild {
    init: fn(),
    storage: MappedStorage,
}

#[derive(Debug)]
pub struct CompiledProgram {
    pub storage: Arc<MappedStorage>,
    init: fn(),
    step: fn() -> f32,
}

impl CompiledProgram {
    pub fn init(&mut self) {
        (self.init)()
    }

    pub fn step(&mut self) -> f32 {
        (self.step)()
    }

    pub fn set_f32(&mut self, name: &str, value: f32) {
        let entry = self.storage.get(name).unwrap();
        assert_eq!(entry.ty, types::F32);
        assert_eq!(entry.kind, StorageEntryKind::External);
        // SAFETY
        // - ptr points to a valid, aligned f32.
        // - The access is synchronized.
        //
        // This is guaranteed by asserting StorageEntry::ty value, MappedStorage
        // invariants and the fact that this function takes a mutable reference.
        unsafe {
            (entry.ptr as *mut f32).write(value);
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
        let InitBuild { init, storage } = self.build_init(program)?;

        let storage = Arc::new(storage);
        let step = self.build_step(program, Arc::clone(&storage))?;

        Ok(CompiledProgram {
            storage,
            init,
            step,
        })
    }

    fn build_init(&mut self, program: &Program) -> anyhow::Result<InitBuild> {
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

        let mut storage_tvs = Vec::<(String, TypedValue, StorageEntryKind)>::new();
        for StateEntry { name, init, .. } in &program.state().entries {
            let mut stack = VarStack::new();
            let mut recursor = Recursor::new(&mut builder, &mut stack, None);

            let init_v = recursor.recurse(init);
            storage_tvs.push((name.name.clone(), init_v, StorageEntryKind::Internal));
        }
        for InputEntry { name, default, .. } in &program.inputs().entries {
            let mut stack = VarStack::new();
            let mut recursor = Recursor::new(&mut builder, &mut stack, None);

            let init_v = recursor.recurse(
                &default
                    .clone()
                    .unwrap_or(Literal {
                        value: LiteralValue::Float(0.0),
                        span: 0..0,
                    })
                    .into(),
            );
            storage_tvs.push((name.name.clone(), init_v, StorageEntryKind::External));
        }
        let mapped_storage = Self::build_state_storage(&storage_tvs)?;

        for (name, ptr_v) in mapped_storage.typed_values() {
            let init_v = storage_tvs
                .iter()
                .find_map(|(id, init_v, _kind)| (id == name).then_some(*init_v))
                .unwrap();

            ptr_v.assign(&mut builder, init_v).unwrap();
        }

        builder.ins().return_(&[]);

        println!("init:\n{}", builder.func.display());
        builder.finalize();

        self.module.define_function(id, &mut self.module_ctx)?;

        self.module.clear_context(&mut self.module_ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        // SAFETY
        // - The function signature matches.
        //
        // Guaranteed within this function.
        let func = unsafe { std::mem::transmute::<*const u8, fn()>(code) };

        Ok(InitBuild {
            init: func,
            storage: mapped_storage,
        })
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
        for (name, ref_tv) in mapped_storage.typed_values() {
            stack.set(name.to_string(), ref_tv);
        }

        let retss = TypedStackSlot::float()(&mut builder);

        let mut recursor = Recursor::new(&mut builder, &mut stack, Some(retss));
        for expr in &program.step().block.exprs {
            recursor.recurse(expr);
        }

        let read_ret = TypedValue::stack_load(&mut builder, retss).value().unwrap();
        builder.ins().return_(&[read_ret]);

        println!("step:\n{}", builder.func.display());
        builder.finalize();

        self.module.define_function(id, &mut self.module_ctx)?;

        self.module.clear_context(&mut self.module_ctx);
        self.module.finalize_definitions()?;

        let code = self.module.get_finalized_function(id);

        // SAFETY
        // - The function signature matches.
        //
        // Guaranteed within this function.
        let func = unsafe { std::mem::transmute::<*const u8, fn() -> f32>(code) };

        Ok(func)
    }

    fn build_state_storage(
        entries: &[(String, TypedValue, StorageEntryKind)],
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

        for (name, tv, kind) in entries {
            let offset = offsets.get(name).unwrap();
            state_mapping.insert(name.clone(), unsafe {
                StorageEntry {
                    ty: tv.cl_type()?,
                    kind: *kind,
                    ptr: storage.get(*offset).as_ptr() as *mut u8,
                }
            });
        }

        // SAFETY: See MappedStorage::new invariants
        //
        // This is guaranteed by the above algorithm.
        Ok(unsafe { MappedStorage::new(state_mapping, storage) })
    }
}
