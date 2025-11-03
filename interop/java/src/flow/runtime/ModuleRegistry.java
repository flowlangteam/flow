package flow.runtime;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

/**
 * Registry for loaded Flow modules
 * Manages module discovery, loading, and caching
 */
class ModuleRegistry {
    private final Map<String, Module> modules = new HashMap<>();
    private ClassLoader moduleClassLoader;
    
    void init() {
        // Set up class loader for Flow modules
        // By default, use the current classpath
        moduleClassLoader = ModuleRegistry.class.getClassLoader();
    }
    
    /**
     * Load a module by name
     * First checks cache, then attempts to load from classpath
     */
    Module load(String moduleName) {
        // Check cache
        if (modules.containsKey(moduleName)) {
            return modules.get(moduleName);
        }
        
        try {
            // Try to load the class
            // Flow modules are compiled with capitalized names
            String className = capitalize(moduleName);
            Class<?> moduleClass = moduleClassLoader.loadClass(className);
            
            // Create and cache module
            Module module = new Module(moduleName, moduleClass);
            modules.put(moduleName, module);
            
            return module;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Module '" + moduleName + "' not found in classpath", e);
        }
    }
    
    /**
     * Register a module explicitly
     */
    void register(String name, Module module) {
        modules.put(name, module);
    }
    
    /**
     * Add a directory or JAR to the module search path
     */
    void addSearchPath(String path) {
        try {
            File file = new File(path);
            URL url = file.toURI().toURL();
            
            // Create new class loader with additional path
            URLClassLoader newLoader = new URLClassLoader(
                new URL[]{url},
                moduleClassLoader
            );
            moduleClassLoader = newLoader;
        } catch (Exception e) {
            throw new RuntimeException("Failed to add search path: " + path, e);
        }
    }
    
    /**
     * Clear module cache
     */
    void cleanup() {
        modules.clear();
    }
    
    /**
     * Get number of loaded modules
     */
    int getLoadedModuleCount() {
        return modules.size();
    }
    
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
