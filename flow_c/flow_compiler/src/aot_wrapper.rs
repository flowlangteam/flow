use crate::jit_engine::JitEngine;
use crate::{
    CallingConvention, CompilationTarget, CompiledModule, CompilerConfig, CompilerOutput,
    FlowCompiler, FunctionInfo, FunctionSignature, Result, TypeInfo,
};
use flow_ast::Program;
use std::collections::HashMap;

pub struct AotCompilerWrapper {
    jit_engine: JitEngine,
}

impl AotCompilerWrapper {
    pub fn new() -> Self {
        Self {
            jit_engine: JitEngine::new(),
        }
    }
}

impl FlowCompiler for AotCompilerWrapper {
    fn compile_program(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        self.jit_engine.apply_compiler_config(config)?;

        // First, compile with JIT to get the function pointer
        match self.jit_engine.compile(program) {
            Ok(main_func_ptr) => {
                // TODO: Convert JIT-compiled code to object file
                // For now, we'll create a placeholder object file
                let object_bytes = self.jit_to_object(main_func_ptr, program)?;

                Ok(CompilerOutput::Object(object_bytes))
            }
            Err(e) => Err(e),
        }
    }

    fn compile_module(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompiledModule> {
        self.jit_engine.apply_compiler_config(config)?;

        match self.jit_engine.compile_module(program) {
            Ok(()) => {
                // Extract module information from the module registry
                let namespace = program
                    .namespace
                    .as_ref()
                    .map(|ns| ns.namespace.clone())
                    .unwrap_or_else(|| "main".to_string());

                let mut functions = HashMap::new();
                let mut types = HashMap::new();
                let mut dependencies = Vec::new();

                // Get the module that was just registered
                if let Some(external_module) =
                    self.jit_engine.get_module_registry().get_module(&namespace)
                {
                    // Convert ExternalFunction to FunctionInfo
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
                            is_public: true, // Only public functions are in the registry
                            namespace: external_func.namespace.clone(),
                        };

                        functions.insert(external_func.name.clone(), function_info);
                    }
                }

                // Extract type information from the AST (since types aren't in module registry yet)
                for item in &program.items {
                    match item {
                        flow_ast::Item::Struct(struct_def) => {
                            let (size, alignment) = self
                                .jit_engine
                                .struct_layout_info(&struct_def.name)
                                .unwrap_or_else(|| {
                                    let pointer = self.jit_engine.pointer_size();
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
                    target: CompilationTarget::Native,
                    data: Vec::new(), // Module compilation doesn't produce final object data until linking
                })
            }
            Err(e) => Err(e),
        }
    }

    fn link_modules(
        &mut self,
        modules: &[CompiledModule],
        _config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        // TODO: Implement module linking by combining JIT-compiled modules into a single executable
        // For now, create a simple combined object file
        let mut combined_data = Vec::new();

        for module in modules {
            combined_data.extend_from_slice(&module.data);
        }

        // Add some basic ELF header data (placeholder)
        if combined_data.is_empty() {
            // Create a minimal object file
            combined_data = vec![0x7f, 0x45, 0x4c, 0x46]; // ELF magic number
            combined_data.extend_from_slice(&vec![0; 324]); // Minimal ELF header
        }

        Ok(CompilerOutput::Object(combined_data))
    }

    fn target_info(&self) -> CompilationTarget {
        CompilationTarget::Native
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
            "modules".to_string(),
        ]
    }
}

impl AotCompilerWrapper {
    /// Convert JIT-compiled function to object file bytes
    fn jit_to_object(&self, _main_func_ptr: *const u8, _program: &Program) -> Result<Vec<u8>> {
        // TODO: Implement proper JIT-to-object conversion
        // This could involve:
        // 1. Extracting the machine code from the JIT-compiled function
        // 2. Creating an ELF/PE/Mach-O object file with proper headers
        // 3. Adding symbol tables and relocation information
        // 4. Handling dependencies and imports

        // For now, create a placeholder object file
        let mut object_data = Vec::new();

        // ELF magic number
        object_data.extend_from_slice(&[0x7f, 0x45, 0x4c, 0x46]);

        // Add minimal ELF header (64-bit, little-endian, current version)
        object_data.extend_from_slice(&[
            0x02, 0x01, 0x01, 0x00, // EI_CLASS, EI_DATA, EI_VERSION, EI_PAD
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // EI_PAD continued
            0x01, 0x00, // e_type: ET_REL (relocatable file)
            0x3e, 0x00, // e_machine: EM_X86_64
        ]);

        // Fill with zeros to make a valid minimal object file
        object_data.resize(320, 0);

        Ok(object_data)
    }
}
