#![allow(clippy::new_without_default)]
#![allow(clippy::needless_if)]
#![allow(clippy::too_many_arguments)]

use flow_ast::Program;
use std::collections::HashMap;
use thiserror::Error;

mod aot_wrapper;
mod java_wrapper;
mod jit_engine;
mod jit_wrapper;

pub use jit_engine::{ExternalFunction, ExternalModule, JitEngine, ModuleRegistry};

/// Unified error type for all compilation backends
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("JIT compilation error: {0}")]
    JitError(String),

    #[error("AOT compilation error: {0}")]
    AotError(String),

    #[error("Transpilation error: {0}")]
    TranspileError(String),

    #[error("Invalid target: {0}")]
    InvalidTarget(String),

    #[error("Code generation error: {0}")]
    CodeGenError(String),

    #[error("Module resolution error: {0}")]
    ModuleError(String),
}

pub type Result<T> = std::result::Result<T, CompilerError>;

/// Configuration for compilation
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    /// Target platform/language
    pub target: CompilationTarget,
    /// Optimization level (0-3)
    pub optimization_level: u8,
    /// Debug information generation
    pub debug_info: bool,
    /// Output directory
    pub output_dir: Option<String>,
    /// Additional flags
    pub flags: HashMap<String, String>,
}

/// Supported compilation targets
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilationTarget {
    /// Just-in-time compilation (runtime execution)
    Jit,
    /// Ahead-of-time compilation to native object code
    Native,
    /// Transpile to Java bytecode
    JavaBytecode,
    /// Transpile to Python source code (future)
    Python,
    /// Transpile to JavaScript source code (future)
    JavaScript,
    /// Transpile to C source code (future)
    C,
    /// Transpile to Rust source code (future)
    Rust,
    /// Transpile to WebAssembly (future)
    WebAssembly,
}

/// Output from compilation
#[derive(Debug)]
pub enum CompilerOutput {
    /// JIT: Function pointer that can be executed immediately
    Jit(Box<dyn std::any::Any>),
    /// AOT: Object code bytes that can be linked
    Object(Vec<u8>),
    /// Transpiler: Source code or bytecode as bytes
    Code(Vec<u8>),
    /// Module: Library that can be linked with other modules
    Module(CompiledModule),
}

/// A compiled module that can be linked with other modules
#[derive(Debug)]
pub struct CompiledModule {
    pub name: String,
    pub functions: HashMap<String, FunctionInfo>,
    pub types: HashMap<String, TypeInfo>,
    pub dependencies: Vec<String>,
    pub target: CompilationTarget,
    pub data: Vec<u8>,
    pub entry_symbol: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub mangled_name: String,
    pub signature: FunctionSignature,
    pub is_public: bool,
    pub namespace: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<flow_ast::Type>,
    pub return_type: Option<flow_ast::Type>,
    pub calling_convention: CallingConvention,
}

#[derive(Debug, Clone)]
pub enum CallingConvention {
    System,
    Fast,
    Cold,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub size: usize,
    pub alignment: usize,
    pub is_public: bool,
}

/// Main unified compiler interface
pub trait FlowCompiler {
    /// Compile a complete program
    fn compile_program(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompilerOutput>;

    /// Compile a module (library without main function)
    fn compile_module(
        &mut self,
        program: &Program,
        config: &CompilerConfig,
    ) -> Result<CompiledModule>;

    /// Link multiple compiled modules together
    fn link_modules(
        &mut self,
        modules: &[CompiledModule],
        config: &CompilerConfig,
    ) -> Result<CompilerOutput>;

    /// Get information about the compiler target
    fn target_info(&self) -> CompilationTarget;

    /// Get supported features for this compiler
    fn supported_features(&self) -> Vec<String>;

    /// Validate a program without compilation (optional optimization)
    fn validate_program(&self, _program: &Program) -> Result<()> {
        // Default implementation - compilers can override for faster validation
        Ok(())
    }
}

/// Factory for creating appropriate compilers
pub struct CompilerFactory;

impl CompilerFactory {
    /// Create a compiler for the specified target
    pub fn create_compiler(target: CompilationTarget) -> Result<Box<dyn FlowCompiler>> {
        match target {
            CompilationTarget::Jit => Ok(Box::new(JitCompilerWrapper::new())),
            CompilationTarget::Native => Ok(Box::new(AotCompilerWrapper::new())),
            CompilationTarget::JavaBytecode => Ok(Box::new(JavaTranspilerWrapper::new())),
            CompilationTarget::Python => Err(CompilerError::InvalidTarget(
                "Python transpiler not yet implemented".to_string(),
            )),
            CompilationTarget::JavaScript => Err(CompilerError::InvalidTarget(
                "JavaScript transpiler not yet implemented".to_string(),
            )),
            CompilationTarget::C => Err(CompilerError::InvalidTarget(
                "C transpiler not yet implemented".to_string(),
            )),
            CompilationTarget::Rust => Err(CompilerError::InvalidTarget(
                "Rust transpiler not yet implemented".to_string(),
            )),
            CompilationTarget::WebAssembly => Err(CompilerError::InvalidTarget(
                "WebAssembly transpiler not yet implemented".to_string(),
            )),
        }
    }

    /// Get all supported targets
    pub fn supported_targets() -> Vec<CompilationTarget> {
        vec![
            CompilationTarget::Jit,
            CompilationTarget::Native,
            CompilationTarget::JavaBytecode,
        ]
    }

    /// Check if a target is supported
    pub fn is_target_supported(target: &CompilationTarget) -> bool {
        Self::supported_targets().contains(target)
    }
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            target: CompilationTarget::Jit,
            optimization_level: 1,
            debug_info: false,
            output_dir: None,
            flags: HashMap::new(),
        }
    }
}

impl CompilerConfig {
    pub fn new(target: CompilationTarget) -> Self {
        Self {
            target,
            ..Default::default()
        }
    }

    pub fn with_optimization(mut self, level: u8) -> Self {
        self.optimization_level = level.min(3);
        self
    }

    pub fn with_debug_info(mut self, debug: bool) -> Self {
        self.debug_info = debug;
        self
    }

    pub fn with_output_dir(mut self, dir: impl Into<String>) -> Self {
        self.output_dir = Some(dir.into());
        self
    }

    pub fn with_flag(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.flags.insert(key.into(), value.into());
        self
    }
}

// Use declarations for the wrapper implementations
pub use aot_wrapper::AotCompilerWrapper;
pub use java_wrapper::JavaTranspilerWrapper;
pub use jit_wrapper::JitCompilerWrapper;

/// High-level convenience API for quick compilation
pub struct FlowCompilerBuilder {
    config: CompilerConfig,
}

impl FlowCompilerBuilder {
    pub fn new() -> Self {
        Self {
            config: CompilerConfig::default(),
        }
    }

    pub fn target(mut self, target: CompilationTarget) -> Self {
        self.config.target = target;
        self
    }

    pub fn optimization_level(mut self, level: u8) -> Self {
        self.config.optimization_level = level.min(3);
        self
    }

    pub fn debug_info(mut self, debug: bool) -> Self {
        self.config.debug_info = debug;
        self
    }

    pub fn output_dir(mut self, dir: impl Into<String>) -> Self {
        self.config.output_dir = Some(dir.into());
        self
    }

    pub fn flag(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config.flags.insert(key.into(), value.into());
        self
    }

    pub fn build(self) -> Result<Box<dyn FlowCompiler>> {
        CompilerFactory::create_compiler(self.config.target)
    }

    pub fn compile_program(self, program: &Program) -> Result<CompilerOutput> {
        let config = self.config.clone();
        let mut compiler = self.build()?;
        compiler.compile_program(program, &config)
    }

    pub fn compile_module(self, program: &Program) -> Result<CompiledModule> {
        let config = self.config.clone();
        let mut compiler = self.build()?;
        compiler.compile_module(program, &config)
    }
}

impl Default for FlowCompilerBuilder {
    fn default() -> Self {
        Self::new()
    }
}
