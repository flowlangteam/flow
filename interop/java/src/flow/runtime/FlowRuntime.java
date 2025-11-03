package flow.runtime;

/**
 * Flow Runtime - Core runtime support for Flow code running on the JVM
 * 
 * Provides:
 * - Memory management utilities
 * - Type conversions between Flow and Java
 * - Function call dispatch
 * - Module loading and caching
 */
public class FlowRuntime {
    private static boolean initialized = false;
    private static final ModuleRegistry moduleRegistry = new ModuleRegistry();
    
    /**
     * Initialize the Flow runtime
     * Must be called before any Flow code is executed
     */
    public static void init() {
        if (initialized) {
            return;
        }
        
        System.out.println("Flow Runtime v0.1.0 initializing...");
        
        // Initialize module registry
        moduleRegistry.init();
        
        // Set up signal handlers
        setupSignalHandlers();
        
        initialized = true;
        System.out.println("Flow Runtime initialized");
    }
    
    /**
     * Load a Flow module by name
     * Searches for compiled .class files or transpiled bytecode
     */
    public static Module loadModule(String moduleName) {
        if (!initialized) {
            throw new IllegalStateException("Flow runtime not initialized. Call FlowRuntime.init() first");
        }
        
        return moduleRegistry.load(moduleName);
    }
    
    /**
     * Register a Flow module explicitly
     */
    public static void registerModule(String name, Module module) {
        moduleRegistry.register(name, module);
    }
    
    /**
     * Shutdown the runtime and cleanup resources
     */
    public static void shutdown() {
        if (!initialized) {
            return;
        }
        
        moduleRegistry.cleanup();
        initialized = false;
        System.out.println("Flow Runtime shutdown");
    }
    
    /**
     * Convert Flow value to Java object
     */
    public static Object toJava(Object flowValue) {
        // Flow values are already compatible with Java
        // This method is for future type wrapping if needed
        return flowValue;
    }
    
    /**
     * Convert Java object to Flow value
     */
    public static Object toFlow(Object javaValue) {
        // Java values are already compatible with Flow
        // This method is for future type wrapping if needed
        return javaValue;
    }
    
    private static void setupSignalHandlers() {
        // Add shutdown hook to cleanup on JVM exit
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            shutdown();
        }));
    }
    
    /**
     * Get runtime version
     */
    public static String version() {
        return "0.1.0";
    }
    
    /**
     * Check if runtime is initialized
     */
    public static boolean isInitialized() {
        return initialized;
    }
}
