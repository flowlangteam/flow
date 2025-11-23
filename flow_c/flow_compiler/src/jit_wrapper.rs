use crate::jit_engine::JitEngine;
use crate::{
    CallingConvention, CompilationTarget, CompiledModule, CompilerConfig, CompilerError,
    CompilerOutput, FlowCompiler, FunctionInfo, FunctionSignature, Result, TypeInfo,
};
use flow_ast::Program;
use std::collections::HashMap;

pub struct JitCompilerWrapper {
    compiler: JitEngine,
}

impl JitCompilerWrapper {
    pub fn new() -> Self {
        Self {
            compiler: JitEngine::new(),
        }
    }
}

impl FlowCompiler for JitCompilerWrapper {
    fn compile_program(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        self.compiler.apply_compiler_config(config)?;

        match self.compiler.compile(program) {
            Ok(func_ptr) => {
                let boxed_ptr = Box::new(func_ptr) as Box<dyn std::any::Any>;
                Ok(CompilerOutput::Jit(boxed_ptr))
            }
            Err(e) => Err(e),
        }
    }

    fn compile_module(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompiledModule> {
        self.compiler.apply_compiler_config(config)?;

        match self.compiler.compile_module(program) {
            Ok(()) => {
                let namespace = program
                    .namespace
                    .as_ref()
                    .map(|ns| ns.namespace.clone())
                    .unwrap_or_else(|| "main".to_string());

                let mut functions = HashMap::new();
                let mut types = HashMap::new();
                let mut dependencies = Vec::new();

                if let Some(external_module) =
                    self.compiler.get_module_registry().get_module(&namespace)
                {
                    for external_func in &external_module.functions {
                        let signature = FunctionSignature {
                            params: external_func.params.clone(),
                            return_type: external_func.return_type.clone(),
                            calling_convention: CallingConvention::System,
                        };

                        let function_info = FunctionInfo {
                            name: external_func.name.clone(),
                            mangled_name: external_func.mangled_name.clone(),
                            signature,
                            is_public: true,
                            namespace: external_func.namespace.clone(),
                        };

                        functions.insert(external_func.name.clone(), function_info);
                    }
                }

                for item in &program.items {
                    match item {
                        flow_ast::Item::Struct(struct_def) => {
                            let (size, alignment) = self
                                .compiler
                                .struct_layout_info(&struct_def.name)
                                .unwrap_or_else(|| {
                                    let pointer = self.compiler.pointer_size();
                                    (pointer, pointer)
                                });

                            let type_info = TypeInfo {
                                name: struct_def.name.clone(),
                                size,
                                alignment,
                                is_public: struct_def.is_pub,
                            };

                            types.insert(struct_def.name.clone(), type_info);
                        }
                        flow_ast::Item::Import(import) => {
                            dependencies.push(import.path.join("::"));
                        }
                        flow_ast::Item::Attribute(_) => {}
                        _ => {}
                    }
                }

                let module_name = program
                    .namespace
                    .as_ref()
                    .map(|ns| ns.namespace.clone())
                    .unwrap_or_else(|| "main".to_string());

                Ok(CompiledModule {
                    name: module_name,
                    functions,
                    types,
                    dependencies,
                    target: CompilationTarget::Jit,
                    data: Vec::new(),
                    entry_symbol: None,
                })
            }
            Err(e) => Err(e),
        }
    }

    fn link_modules(
        &mut self,
        _modules: &[CompiledModule],
        _config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        Err(CompilerError::JitError(
            "Module linking not supported for JIT compilation - compile modules together instead"
                .to_string(),
        ))
    }

    fn target_info(&self) -> CompilationTarget {
        CompilationTarget::Jit
    }

    fn supported_features(&self) -> Vec<String> {
        vec![
            "functions".to_string(),
            "structs".to_string(),
            "conditionals".to_string(),
            "loops".to_string(),
            "arithmetic".to_string(),
            "memory_management".to_string(),
            "pattern_matching".to_string(),
            "lambdas".to_string(),
        ]
    }

    fn validate_program(&self, program: &Program) -> Result<()> {
        let has_main = program.items.iter().any(|item| {
            if let flow_ast::Item::Function(func) = item {
                func.name == "main"
            } else {
                false
            }
        });

        if !has_main {
            return Err(CompilerError::JitError(
                "No main function found".to_string(),
            ));
        }

        Ok(())
    }
}

impl Default for JitCompilerWrapper {
    fn default() -> Self {
        Self::new()
    }
}
