use crate::jit_engine::{ObjectCompileOutput, ObjectEngine};
use crate::{
    CallingConvention, CompilationTarget, CompiledModule, CompilerConfig, CompilerOutput,
    FlowCompiler, FunctionInfo, FunctionSignature, Result, TypeInfo,
};
use flow_ast::Program;
use std::collections::HashMap;
use std::io::Write;
use std::process::Command;

use tempfile::Builder;

pub struct AotCompilerWrapper {
    object_engine: ObjectEngine,
}

impl AotCompilerWrapper {
    pub fn new() -> Self {
        Self {
            object_engine: ObjectEngine::new(),
        }
    }
}

impl FlowCompiler for AotCompilerWrapper {
    fn compile_program(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        self.object_engine.apply_compiler_config(config)?;

        let artifact = self.object_engine.compile_program(program)?;
        let module = self.artifact_to_compiled_module(program, artifact);
        Ok(CompilerOutput::Module(module))
    }

    fn compile_module(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompiledModule> {
        self.object_engine.apply_compiler_config(config)?;

        let artifact = self.object_engine.compile_module(program)?;
        Ok(self.artifact_to_compiled_module(program, artifact))
    }

    fn link_modules(
        &mut self,
        modules: &[CompiledModule],
        config: &CompilerConfig,
    ) -> Result<CompilerOutput> {
        if modules.is_empty() {
            return Err(crate::CompilerError::AotError(
                "No modules provided for linking".to_string(),
            ));
        }

        let linker = config
            .flags
            .get("linker")
            .cloned()
            .unwrap_or_else(|| "cc".to_string());

        let mut temp_objects = Vec::with_capacity(modules.len());
        for module in modules {
            let mut temp = Builder::new()
                .prefix("flow_obj_")
                .suffix(".o")
                .tempfile()
                .map_err(|e| {
                    crate::CompilerError::AotError(format!(
                        "Failed to create temporary object file: {}",
                        e
                    ))
                })?;

            temp.write_all(&module.data).map_err(|e| {
                crate::CompilerError::AotError(format!(
                    "Failed to write temporary object file: {}",
                    e
                ))
            })?;

            temp_objects.push(temp);
        }

        let output_suffix = if cfg!(target_os = "windows") {
            ".exe"
        } else {
            ""
        };

        let output_file = Builder::new()
            .prefix("flow_out_")
            .suffix(output_suffix)
            .tempfile()
            .map_err(|e| {
                crate::CompilerError::AotError(format!(
                    "Failed to create temporary output file: {}",
                    e
                ))
            })?;
        let output_path = output_file.path().to_path_buf();

        let mut command = Command::new(&linker);
        command.arg("-o").arg(&output_path);

        for temp in &temp_objects {
            command.arg(temp.path());
        }

        if let Some(entry) = modules
            .iter()
            .find_map(|module| module.entry_symbol.clone())
        {
            let platform_entry = if cfg!(target_os = "macos") {
                if entry.starts_with('_') {
                    entry
                } else {
                    format!("_{}", entry)
                }
            } else {
                entry
            };

            command.arg(format!("-Wl,-e,{}", platform_entry));
        }

        let output = command.output().map_err(|e| {
            crate::CompilerError::AotError(format!("Failed to execute linker '{}': {}", linker, e))
        })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::CompilerError::AotError(format!(
                "Linker '{}' failed: {}",
                linker,
                stderr.trim()
            )));
        }

        let binary = std::fs::read(&output_path).map_err(|e| {
            crate::CompilerError::AotError(format!(
                "Failed to read linked output '{}': {}",
                output_path.display(),
                e
            ))
        })?;

        Ok(CompilerOutput::Object(binary))
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
    fn artifact_to_compiled_module(
        &self,
        program: &Program,
        artifact: ObjectCompileOutput,
    ) -> CompiledModule {
        let namespace = artifact
            .namespace
            .clone()
            .or_else(|| program.namespace.as_ref().map(|ns| ns.namespace.clone()))
            .unwrap_or_else(|| "main".to_string());

        let mut functions = HashMap::new();
        if let Some(external_module) = self
            .object_engine
            .get_module_registry()
            .get_module(&namespace)
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

        let pointer_fallback = self.object_engine.pointer_size();
        let mut types = HashMap::new();
        let mut dependencies = Vec::new();

        for item in &program.items {
            match item {
                flow_ast::Item::Struct(struct_def) => {
                    let (size, alignment) = artifact
                        .struct_layouts
                        .get(&struct_def.name)
                        .cloned()
                        .unwrap_or((pointer_fallback, pointer_fallback));

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

        CompiledModule {
            name: namespace,
            functions,
            types,
            dependencies,
            target: CompilationTarget::Native,
            data: artifact.object_bytes,
            entry_symbol: artifact.entry_symbol,
        }
    }
}
