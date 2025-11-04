use flow_ast::*;
use flow_analyzer::Analyzer;
use flow_codegen::Compiler;
use std::collections::HashMap;
use std::path::PathBuf;

pub struct ModuleManager {
    analyzer: Analyzer,
    compiler: Compiler,
    compiled_modules: HashMap<String, CompiledModule>,
}

#[derive(Debug, Clone)]
pub struct CompiledModule {
    namespace: String,
    filename: String,
    functions: HashMap<String, *const u8>,
    types: HashMap<String, TypeInfo>,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    name: String,
    size: usize,
    layout: Vec<(String, flow_ast::Type, usize)>, // field_name, type, offset
}

impl ModuleManager {
    pub fn new() -> Self {
        Self {
            analyzer: Analyzer::new(),
            compiler: Compiler::new(),
            compiled_modules: HashMap::new(),
        }
    }
    
    pub fn add_module_search_path(&mut self, path: PathBuf) {
        self.analyzer.add_module_search_path(path);
    }
    
    pub fn set_source_context(&mut self, source: String, file_path: String) {
        self.analyzer.set_source_context(source, file_path);
    }
    
    pub fn compile_program(&mut self, main_program: &Program) -> Result<*const u8, (String, Option<Vec<flow_analyzer::AnalysisError>>)> {
        // Step 1: Analyze the main program and all its dependencies
        match self.analyzer.analyze(main_program) {
            Ok(()) => {},
            Err(errors) => {
                return Err(("Analysis failed".to_string(), Some(errors)));
            }
        }
        
        // Step 2: Get all parsed modules from the analyzer
        let parsed_modules = self.analyzer.get_parsed_modules().clone();
        
        // Step 3: Compile all library modules first
        for (module_key, module_program) in &parsed_modules {
            self.compiler.compile_module(module_program)
                .map_err(|e| (format!("Compilation error in module {}: {}", module_key, e), None))?;
            
            // Store compiled module info
            if let Some(namespace_decl) = &module_program.namespace {
                let compiled_module = CompiledModule {
                    namespace: namespace_decl.namespace.clone(),
                    filename: namespace_decl.filename.clone(),
                    functions: HashMap::new(), // TODO: Populate with actual compiled function pointers
                    types: HashMap::new(), // TODO: Populate with type information from structs
                };
                self.compiled_modules.insert(module_key.clone(), compiled_module);
            }
        }
        
        // Step 4: Compile the main program
        let main_func_ptr = self.compiler.compile(main_program)
            .map_err(|e| (format!("Compilation error in main program: {}", e), None))?;
        
        Ok(main_func_ptr)
    }
    
    pub fn resolve_function(&self, namespace: &str, function_name: &str) -> Option<*const u8> {
        let module_key = namespace;
        if let Some(module) = self.compiled_modules.get(module_key) {
            module.functions.get(function_name).copied()
        } else {
            None
        }
    }
    
    pub fn get_warnings(&self) -> &[Warning] {
        self.analyzer.get_warnings()
    }
    
    pub fn get_analysis_errors(&self) -> &[flow_analyzer::AnalysisError] {
        self.analyzer.get_errors()
    }
}