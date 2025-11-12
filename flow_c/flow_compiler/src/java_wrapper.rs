use crate::{
    CallingConvention, CompilationTarget, CompiledModule, CompilerConfig, CompilerError,
    CompilerOutput, FlowCompiler, FunctionInfo, FunctionSignature, Result, TypeInfo,
};
use flow_ast::Program;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::collections::HashMap;

pub struct JavaTranspilerWrapper {
    transpiler: JavaTranspiler,
}

impl JavaTranspilerWrapper {
    pub fn new() -> Self {
        Self {
            transpiler: JavaTranspiler::new("FlowProgram"),
        }
    }

    pub fn with_class_name(class_name: impl Into<String>) -> Self {
        Self {
            transpiler: JavaTranspiler::new(class_name),
        }
    }
}

impl FlowCompiler for JavaTranspilerWrapper {
    fn compile_program(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        // Apply configuration to the transpiler
        if config.debug_info {
            // TODO: Enable debug information in Java bytecode
        }

        match self.transpiler.transpile(program) {
            Ok(bytecode) => Ok(CompilerOutput::Code(bytecode)),
            Err(e) => Err(CompilerError::TranspileError(format!(
                "Java transpilation failed: {}",
                e
            ))),
        }
    }

    fn compile_module(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompiledModule> {
        // Apply configuration
        if config.debug_info {
            // TODO: Enable debug information in Java bytecode
        }

        match self.transpiler.transpile(program) {
            Ok(bytecode) => {
                // Extract module information from the compiled program
                let mut functions = HashMap::new();
                let mut types = HashMap::new();
                let mut dependencies = Vec::new();

                // Collect function information
                for item in &program.items {
                    match item {
                        flow_ast::Item::Function(func) => {
                            let signature = FunctionSignature {
                                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                                return_type: func.return_type.clone(),
                                calling_convention: CallingConvention::System,
                            };

                            // Java methods are mangled according to JVM rules
                            let mangled_name = format!(
                                "{}({}){}",
                                func.name,
                                // TODO: Convert Flow types to JVM type descriptors
                                "",  // Parameters descriptor
                                "V"  // Return type descriptor (void for now)
                            );

                            let function_info = FunctionInfo {
                                name: func.name.clone(),
                                mangled_name,
                                signature,
                                is_public: func.is_pub,
                                namespace: program
                                    .namespace
                                    .as_ref()
                                    .map(|ns| ns.namespace.clone()),
                            };

                            functions.insert(func.name.clone(), function_info);
                        }
                        flow_ast::Item::Struct(struct_def) => {
                            // Java structs become classes
                            let type_info = TypeInfo {
                                name: struct_def.name.clone(),
                                size: 0, // Java objects are managed by GC, size not directly relevant
                                alignment: 8, // Standard object alignment
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
                    .unwrap_or_else(|| "FlowProgram".to_string());

                Ok(CompiledModule {
                    name: module_name,
                    functions,
                    types,
                    dependencies,
                    target: CompilationTarget::JavaBytecode,
                    data: bytecode,
                })
            }
            Err(e) => Err(CompilerError::TranspileError(format!(
                "Java module compilation failed: {}",
                e
            ))),
        }
    }

    fn link_modules(
        &mut self,
        modules: &[CompiledModule],
        _config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        // Java bytecode linking is handled by the JVM at runtime
        // For now, we'll create a JAR-like structure with multiple class files

        if modules.is_empty() {
            return Err(CompilerError::TranspileError(
                "No modules to link".to_string(),
            ));
        }

        // Verify all modules are for Java bytecode
        for module in modules {
            if module.target != CompilationTarget::JavaBytecode {
                return Err(CompilerError::TranspileError(format!(
                    "Cannot link module '{}' - incompatible target {:?}",
                    module.name, module.target
                )));
            }
        }

        // For now, just concatenate all bytecode
        // TODO: Implement proper JAR file generation or multi-class output
        let mut combined_data = Vec::new();
        for module in modules {
            combined_data.extend_from_slice(&module.data);
        }

        Ok(CompilerOutput::Code(combined_data))
    }

    fn target_info(&self) -> CompilationTarget {
        CompilationTarget::JavaBytecode
    }

    fn supported_features(&self) -> Vec<String> {
        vec![
            "functions".to_string(),
            "structs".to_string(), // As classes
            "conditionals".to_string(),
            "loops".to_string(),
            "arithmetic".to_string(),
            "gc_memory_management".to_string(),
            "static_methods".to_string(),
            "classes".to_string(),
        ]
    }

    fn validate_program(&self, program: &Program) -> Result<()> {
        // Quick validation for Java bytecode generation

        // Check for main function (required for executable Java programs)
        let has_main = program.items.iter().any(|item| {
            if let flow_ast::Item::Function(func) = item {
                func.name == "main"
            } else {
                false
            }
        });

        if !has_main {
            return Err(CompilerError::TranspileError(
                "No main function found for Java program".to_string(),
            ));
        }

        // TODO: Add Java-specific validation:
        // - Check for Java-incompatible features
        // - Validate class structure
        // - Check method signatures
        // - Validate field access patterns

        Ok(())
    }
}

impl Default for JavaTranspilerWrapper {
    fn default() -> Self {
        Self::new()
    }
}
