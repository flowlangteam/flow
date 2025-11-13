use crate::{FlowCompiler, CompilerConfig, CompilerOutput, CompiledModule, CompilationTarget, Result, CompilerError, FunctionInfo, TypeInfo, FunctionSignature, CallingConvention};
use flow_ast::Program;
use flow_codegen::AOTCompiler;
use std::collections::HashMap;

pub struct AotCompilerWrapper {
    compiler: AOTCompiler,
}

impl AotCompilerWrapper {
    pub fn new() -> Self {
        Self {
            compiler: AOTCompiler::new(),
        }
    }
}

impl FlowCompiler for AotCompilerWrapper {
    fn compile_program(&mut self, program: &Program, config: &CompilerConfig) -> Result<CompilerOutput> {
        // Apply configuration to the underlying compiler
        if config.optimization_level > 1 {
            // TODO: Apply optimization settings to Cranelift
        }
        
        // Create a new compiler for this compilation
        let compiler = AOTCompiler::new();
        match compiler.compile_to_object(program) {
            Ok(object_bytes) => {
                Ok(CompilerOutput::Object(object_bytes))
            }
            Err(e) => Err(CompilerError::AotError(e)),
        }
    }
    
    fn compile_module(&mut self, program: &Program, config: &CompilerConfig) -> Result<CompiledModule> {
        // Apply configuration
        if config.optimization_level > 1 {
            // TODO: Apply optimization settings to Cranelift
        }
        
        match self.compiler.compile_module(program) {
            Ok(()) => {
                // Extract module information from the module registry 
                let namespace = program.namespace.as_ref()
                    .map(|ns| ns.namespace.clone())
                    .unwrap_or_else(|| "main".to_string());
                
                let mut functions = HashMap::new();
                let mut types = HashMap::new();
                let mut dependencies = Vec::new();
                
                // Get the module that was just registered
                if let Some(external_module) = self.compiler.get_module_registry().get_module(&namespace) {
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
                            // TODO: Calculate actual size and alignment
                            let type_info = TypeInfo {
                                name: struct_def.name.clone(),
                                size: 0, // Placeholder - need to compute from fields
                                alignment: 8, // Placeholder
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
                
                let module_name = program.namespace
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
                    entry_symbol: None,
                })
            }
            Err(e) => Err(CompilerError::AotError(e)),
        }
                            let mangled_name = if let Some(namespace) = &program.namespace {
                                format!("{}::{}", namespace.namespace, func.name)
                            } else {
                                func.name.clone()
                            };
                            
                            let function_info = FunctionInfo {
                                name: func.name.clone(),
                                mangled_name,
                                signature,
                                is_public: func.is_pub,
                                namespace: program.namespace.as_ref().map(|ns| ns.namespace.clone()),
                            };
                            
                            functions.insert(func.name.clone(), function_info);
                        }
                        flow_ast::Item::Struct(struct_def) => {
                            // TODO: Calculate actual size and alignment from struct layout
                            let type_info = TypeInfo {
                                name: struct_def.name.clone(),
                                size: 0, // Placeholder - need to compute from fields
                                alignment: 8, // Placeholder
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
                
                let module_name = program.namespace
                    .as_ref()
                    .map(|ns| ns.namespace.clone())
                    .unwrap_or_else(|| "main".to_string());
                
                Ok(CompiledModule {
                    name: module_name,
                    functions,
                    types,
                    dependencies,
                    target: CompilationTarget::Native,
                    data: object_bytes,
                    entry_symbol: None,
                })
            }
            Err(e) => Err(CompilerError::AotError(e)),
        }
    }
    
    fn link_modules(&mut self, modules: &[CompiledModule], _config: &CompilerConfig) -> Result<CompilerOutput> {
        // AOT compilation linking would require a separate linker
        // For now, we'll concatenate the object files and let the system linker handle it
        
        if modules.is_empty() {
            return Err(CompilerError::AotError("No modules to link".to_string()));
        }
        
        // Verify all modules are for the same target
        for module in modules {
            if module.target != CompilationTarget::Native {
                return Err(CompilerError::AotError(
                    format!("Cannot link module '{}' - incompatible target {:?}", module.name, module.target)
                ));
            }
        }
        
        // For now, just return the first module's data
        // TODO: Implement proper linking logic using a system linker or custom linker
        let first_module = &modules[0];
        Ok(CompilerOutput::Object(first_module.data.clone()))
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
            "pattern_matching".to_string(), // Partial support
            "lambdas".to_string(), // Partial support
            "namespaces".to_string(),
            "modules".to_string(),
        ]
    }
    
    fn validate_program(&self, program: &Program) -> Result<()> {
        // Quick validation without full compilation
        
        // For libraries, we don't require a main function
        // For executables, we do
        
        // TODO: Add configuration to specify whether this is a library or executable
        // For now, assume it's an executable if no namespace is specified
        if program.namespace.is_none() {
            let has_main = program.items.iter().any(|item| {
                if let flow_ast::Item::Function(func) = item {
                    func.name == "main"
                } else {
                    false
                }
            });
            
            if !has_main {
                return Err(CompilerError::AotError("No main function found for executable".to_string()));
            }
        }
        
        // TODO: Add more validation checks:
        // - Type checking
        // - Name resolution
        // - Control flow analysis
        // - Dead code elimination opportunities
        
        Ok(())
    }
}

impl Default for AotCompilerWrapper {
    fn default() -> Self {
        Self::new()
    }
}