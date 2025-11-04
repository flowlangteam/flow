use jni::JNIEnv;
use jni::objects::{JClass, JString, JObject};
use jni::sys::{jlong, jdouble, jstring};
use std::ptr;
use std::sync::Mutex;
use std::collections::HashMap;

use flow_parser::Parser;
use flow_codegen::Compiler;
use flow_ast::Item;

// Global state for managing compiled modules
lazy_static::lazy_static! {
    static ref MODULE_CACHE: Mutex<HashMap<String, CompiledModule>> = Mutex::new(HashMap::new());
    static ref NEXT_MODULE_ID: Mutex<u64> = Mutex::new(0);
}

struct CompiledModule {
    id: u64,
    name: String,
    compiler: Compiler,
    functions: Vec<String>,
}

/// Initialize the Flow runtime
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_initializeRuntime(
    _env: JNIEnv,
    _class: JClass,
) {
    // Runtime initialization if needed
    println!("Flow JNI Runtime initialized");
}

/// Compile Flow source code to native code via JIT
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_compileFlow(
    mut env: JNIEnv,
    _class: JClass,
    source: JString,
    module_name: JString,
) -> jlong {
    let source_str: String = match env.get_string(&source) {
        Ok(s) => s.into(),
        Err(e) => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Failed to get source string: {}", e));
            return -1;
        }
    };

    let name_str: String = match env.get_string(&module_name) {
        Ok(s) => s.into(),
        Err(e) => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Failed to get module name: {}", e));
            return -1;
        }
    };

    // Parse the source code
    let mut parser = Parser::new(&source_str);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Parser error: {:?}", e));
            return -1;
        }
    };

    // Compile to native code
    let mut compiler = Compiler::new();

    if let Err(e) = compiler.compile(&ast) {
        let _ = env.throw_new("java/lang/RuntimeException", format!("Compilation error: {:?}", e));
        return -1;
    }

    // Extract function names from AST
    let mut functions = Vec::new();
    for item in &ast.items {
        if let Item::Function(func) = item {
            functions.push(func.name.clone());
        }
    }

    // Generate unique module ID
    let mut next_id = NEXT_MODULE_ID.lock().unwrap();
    let module_id = *next_id;
    *next_id += 1;

    // Store the compiled module
    let module = CompiledModule {
        id: module_id,
        name: name_str.clone(),
        compiler,
        functions,
    };

    let mut cache = MODULE_CACHE.lock().unwrap();
    cache.insert(name_str, module);

    module_id as jlong
}

/// Call a native Flow function by name
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_callNativeFunction(
    mut env: JNIEnv,
    _class: JClass,
    module_id: jlong,
    function_name: JString,
    _args: JObject,
) -> jdouble {
    let func_name: String = match env.get_string(&function_name) {
        Ok(s) => s.into(),
        Err(e) => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Failed to get function name: {}", e));
            return 0.0;
        }
    };

    // Find the module by ID
    let cache = MODULE_CACHE.lock().unwrap();
    let module = match cache.values().find(|m| m.id == module_id as u64) {
        Some(m) => m,
        None => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Module not found: {}", module_id));
            return 0.0;
        }
    };

    // Verify function exists
    if !module.functions.contains(&func_name) {
        let _ = env.throw_new("java/lang/RuntimeException", format!("Function not found: {}", func_name));
        return 0.0;
    }

    // Get the compiled function pointer
    let func_ptr = match module.compiler.get_function(&func_name) {
        Some(ptr) => ptr,
        None => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Function not compiled: {}", func_name));
            return 0.0;
        }
    };

    // @TODO: Implement full argument marshalling for all types
    // Parse args array (for now, support simple cases)
    // TODO: Implement full argument marshalling
    
    // @TODO: Support functions with parameters - currently assumes no-arg functions
    // Call the function (assuming no-arg for now)
    unsafe {
        let func: fn() -> f64 = std::mem::transmute(func_ptr);
        func()
    }
}

/// Get list of functions in a compiled module
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_getFunctions(
    mut env: JNIEnv,
    _class: JClass,
    module_id: jlong,
) -> jstring {
    let cache = MODULE_CACHE.lock().unwrap();
    let module = match cache.values().find(|m| m.id == module_id as u64) {
        Some(m) => m,
        None => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Module not found: {}", module_id));
            return ptr::null_mut();
        }
    };

    // Join function names with commas
    let functions_list = module.functions.join(",");
    
    match env.new_string(&functions_list) {
        Ok(s) => s.into_raw(),
        Err(e) => {
            let _ = env.throw_new("java/lang/RuntimeException", format!("Failed to create string: {}", e));
            ptr::null_mut()
        }
    }
}

/// Unload a compiled module
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_unloadModule(
    _env: JNIEnv,
    _class: JClass,
    module_id: jlong,
) {
    let mut cache = MODULE_CACHE.lock().unwrap();
    cache.retain(|_, m| m.id != module_id as u64);
}

/// Shutdown the Flow runtime
#[no_mangle]
pub extern "system" fn Java_flow_bridge_FlowBridge_shutdownRuntime(
    _env: JNIEnv,
    _class: JClass,
) {
    let mut cache = MODULE_CACHE.lock().unwrap();
    cache.clear();
    println!("Flow JNI Runtime shut down");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_cache() {
        // Basic test to ensure module cache works
        let cache = MODULE_CACHE.lock().unwrap();
        assert_eq!(cache.len(), 0);
    }
}
