package flow.runtime;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents a loaded Flow module
 * Provides access to functions and types defined in the module
 */
public class Module {
    private final String name;
    private final Class<?> moduleClass;
    private final Map<String, Method> functions;
    
    public Module(String name, Class<?> moduleClass) {
        this.name = name;
        this.moduleClass = moduleClass;
        this.functions = new HashMap<>();
        
        // Discover all public static methods (Flow functions are compiled as static)
        discoverFunctions();
    }
    
    /**
     * Call a function in this module
     */
    public Object call(String functionName, Object... args) {
        Method method = functions.get(functionName);
        if (method == null) {
            throw new IllegalArgumentException(
                "Function '" + functionName + "' not found in module '" + name + "'"
            );
        }
        
        try {
            // Flow functions are static methods
            return method.invoke(null, args);
        } catch (Exception e) {
            throw new RuntimeException("Error calling function '" + functionName + "': " + e.getMessage(), e);
        }
    }
    
    /**
     * Call a function and expect a long result
     */
    public long callLong(String functionName, Object... args) {
        Object result = call(functionName, args);
        if (result instanceof Long) {
            return (Long) result;
        }
        throw new ClassCastException("Function '" + functionName + "' did not return a long");
    }
    
    /**
     * Call a function and expect an int result
     */
    public int callInt(String functionName, Object... args) {
        Object result = call(functionName, args);
        if (result instanceof Integer) {
            return (Integer) result;
        }
        throw new ClassCastException("Function '" + functionName + "' did not return an int");
    }
    
    /**
     * Call a function and expect a boolean result
     */
    public boolean callBoolean(String functionName, Object... args) {
        Object result = call(functionName, args);
        if (result instanceof Boolean) {
            return (Boolean) result;
        }
        throw new ClassCastException("Function '" + functionName + "' did not return a boolean");
    }
    
    /**
     * Call a function and expect a String result
     */
    public String callString(String functionName, Object... args) {
        Object result = call(functionName, args);
        if (result instanceof String) {
            return (String) result;
        }
        throw new ClassCastException("Function '" + functionName + "' did not return a String");
    }
    
    /**
     * Check if a function exists in this module
     */
    public boolean hasFunction(String functionName) {
        return functions.containsKey(functionName);
    }
    
    /**
     * Get the module name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Get the underlying Java class
     */
    public Class<?> getModuleClass() {
        return moduleClass;
    }
    
    /**
     * List all available functions
     */
    public String[] listFunctions() {
        return functions.keySet().toArray(new String[0]);
    }
    
    private void discoverFunctions() {
        // Get all public static methods
        for (Method method : moduleClass.getDeclaredMethods()) {
            int modifiers = method.getModifiers();
            if (java.lang.reflect.Modifier.isPublic(modifiers) && 
                java.lang.reflect.Modifier.isStatic(modifiers)) {
                // Skip constructor and special methods
                if (!method.getName().equals("<init>") && !method.getName().equals("<clinit>")) {
                    functions.put(method.getName(), method);
                }
            }
        }
    }
}
