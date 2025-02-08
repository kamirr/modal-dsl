pub mod library;
mod recursor;
mod storage;
mod typed;
mod varstack;

use std::{collections::HashMap, error::Error, ops::Range, sync::Arc};

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        types::F32, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        Signature,
    },
};
use cranelift_codegen::{settings, Context};
use library::Library;
use recursor::Recursor;
use storage::{MappedStorage, StorageBuf, StorageEntry, StorageEntryKind};
use typed::{LoadCache, TypedStackSlot, TypedValue, ValueType};
use varstack::VarStack;

use crate::parse::{inputs::InputEntry, state::StateEntry, Program};

struct InitBuild {
    init: fn(),
    storage: MappedStorage,
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub span: Range<usize>,
    pub msg: String,
}

impl CompileError {
    pub fn new(msg: impl Into<String>, span: Range<usize>) -> Self {
        CompileError {
            span,
            msg: msg.into(),
        }
    }
}

impl<E: Error> From<E> for CompileError {
    fn from(error: E) -> Self {
        CompileError {
            span: 0..0,
            msg: error.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct InputDesc {
    pub name: String,
    pub ty: String,
    pub args: Vec<String>,
}

#[derive(Debug)]
pub struct CompiledProgram {
    pub storage: Arc<MappedStorage>,
    inputs: Vec<InputDesc>,
    init: fn(),
    step: fn() -> f32,
}

impl CompiledProgram {
    pub fn init(self) -> ReadyProgram {
        (self.init)();
        ReadyProgram {
            storage: self.storage,
            inputs: self.inputs,
            step: self.step,
        }
    }

    pub fn inputs(&self) -> &[InputDesc] {
        &self.inputs
    }
}

#[derive(Debug)]
pub struct ReadyProgram {
    pub storage: Arc<MappedStorage>,
    inputs: Vec<InputDesc>,
    step: fn() -> f32,
}

impl ReadyProgram {
    pub fn step(&mut self) -> f32 {
        (self.step)()
    }

    pub fn inputs(&self) -> &[InputDesc] {
        &self.inputs
    }

    pub fn set_f32(&mut self, name: &str, value: f32) {
        let entry = self.storage.get(name).unwrap();
        assert_eq!(entry.abi, ValueType::Float);
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
    stdlib: Library,
    extern_signatures: HashMap<String, Signature>,
}

impl Compiler {
    pub fn new(stdlib: Library) -> anyhow::Result<Self> {
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
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());
        for (name, ptr) in stdlib.symbols() {
            builder.symbol(name, ptr);
        }

        let module = JITModule::new(builder);

        let mut extern_signatures = HashMap::new();
        for (name, func) in stdlib.funcs() {
            let mut sig = module.make_signature();
            for arg in &func.args {
                sig.params.push(AbiParam::new(arg.cl_type()));
            }
            match &func.ret {
                Some(vt) => sig.returns.push(AbiParam::new(vt.cl_type())),
                None => {}
            }
            extern_signatures.insert(name.to_string(), sig);
        }

        let module_ctx = module.make_context();

        Ok(Compiler {
            module,
            module_ctx,
            stdlib,
            extern_signatures,
        })
    }

    pub fn compile(&mut self, program: &Program) -> Result<CompiledProgram, CompileError> {
        let InitBuild { init, storage } = self.build_init(program)?;

        let storage = Arc::new(storage);
        let step = self.build_step(program, Arc::clone(&storage))?;

        let inputs = program
            .inputs()
            .entries
            .iter()
            .map(|InputEntry { name, ty, args, .. }| InputDesc {
                name: name.name.clone(),
                ty: ty.name.clone(),
                args: args.clone(),
            })
            .collect();

        Ok(CompiledProgram {
            storage,
            inputs,
            init,
            step,
        })
    }

    fn build_init(&mut self, program: &Program) -> Result<InitBuild, CompileError> {
        self.module_ctx.func.signature.params = vec![];
        self.module_ctx.func.signature.returns = vec![];

        let id = self.module.declare_function(
            "jit_init",
            Linkage::Export,
            &self.module_ctx.func.signature,
        )?;

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.module_ctx.func, &mut builder_ctx);

        let extern_funs = {
            let mut tmp = HashMap::new();
            for (name, sig) in &self.extern_signatures {
                let callee = self.module.declare_function(&name, Linkage::Import, &sig)?;
                let fun_ref = self.module.declare_func_in_func(callee, builder.func);

                tmp.insert(
                    name.to_string(),
                    (fun_ref, self.stdlib.get_func(name).unwrap().clone()),
                );
            }

            tmp
        };

        let block = builder.create_block();
        builder.seal_block(block);

        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let mut storage_tvs = Vec::<(String, TypedValue, StorageEntryKind)>::new();
        for StateEntry { name, init, .. } in &program.state().entries {
            let mut stack = VarStack::new();
            let mut recursor = Recursor::new(&mut builder, &mut stack, None, extern_funs.clone());

            let init_v = recursor.recurse(init)?;
            storage_tvs.push((name.name.clone(), init_v, StorageEntryKind::Internal));
        }
        for InputEntry { name, .. } in &program.inputs().entries {
            let zero = TypedValue::float(&mut builder, 0.0);
            storage_tvs.push((name.name.clone(), zero.clone(), StorageEntryKind::External));
        }
        let mapped_storage = Self::build_state_storage(&storage_tvs)?;

        for (name, ptr_v) in mapped_storage.typed_values() {
            let init_v = storage_tvs
                .iter()
                .find_map(|(id, init_v, _kind)| (id == name).then_some(init_v.clone()))
                .unwrap();

            LoadCache::default()
                .store(&mut builder, ptr_v, init_v)
                .unwrap();
        }

        builder.ins().return_(&[]);

        log::debug!("init IR:\n{}", builder.func.display());
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
    ) -> Result<fn() -> f32, CompileError> {
        self.module_ctx.func.signature.params = vec![];
        self.module_ctx.func.signature.returns = vec![AbiParam::new(F32)];

        let id = self.module.declare_function(
            "jit_step",
            Linkage::Export,
            &self.module_ctx.func.signature,
        )?;

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.module_ctx.func, &mut builder_ctx);

        let extern_funs = {
            let mut tmp = HashMap::new();
            for (name, sig) in &self.extern_signatures {
                let callee = self.module.declare_function(&name, Linkage::Import, &sig)?;
                let fun_ref = self.module.declare_func_in_func(callee, builder.func);

                tmp.insert(
                    name.to_string(),
                    (fun_ref, self.stdlib.get_func(name).unwrap().clone()),
                );
            }

            tmp
        };

        let block = builder.create_block();
        builder.seal_block(block);

        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let mut stack = VarStack::new();
        for (name, ref_tv) in mapped_storage.typed_values() {
            stack.set(name.to_string(), ref_tv);
        }

        let retss = TypedStackSlot::new(&mut builder, ValueType::Float);

        let mut recursor = Recursor::new(
            &mut builder,
            &mut stack,
            Some(retss.clone()),
            extern_funs.clone(),
        );
        for expr in &program.step().block.exprs {
            recursor.recurse(expr)?;
        }

        let read_ret = retss.load(&mut builder).value(&mut builder);
        builder.ins().return_(&[read_ret]);

        log::debug!("step IR:\n{}", builder.func.display());
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
    ) -> Result<MappedStorage, CompileError> {
        let mut offset = 0usize;
        let mut state_mapping = HashMap::new();
        let mut offsets = HashMap::new();

        for (name, tv, _kind) in entries {
            let abi_type = tv.value_type().cl_type();

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
                    abi: tv.value_type(),
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
