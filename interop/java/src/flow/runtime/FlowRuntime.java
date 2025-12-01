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
    private static java.io.File flowExecutable = null;
    
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

    /**
     * Get the path to the bundled Flow executable.
     * Extracts it from the JAR if necessary.
     */
    public static synchronized java.io.File getFlowExecutable() throws java.io.IOException {
        if (flowExecutable != null && flowExecutable.exists()) {
            return flowExecutable;
        }

        String osName = System.getProperty("os.name").toLowerCase();
        String extension = osName.contains("win") ? ".exe" : "";
        String resourcePath = "/bin/flow" + extension;
        
        java.io.InputStream is = FlowRuntime.class.getResourceAsStream(resourcePath);
        if (is == null) {
            throw new java.io.FileNotFoundException("Flow executable not found in JAR at " + resourcePath);
        }

        java.io.File tempFile = java.io.File.createTempFile("flow", extension);
        tempFile.deleteOnExit();
        if (!tempFile.setExecutable(true)) {
             System.err.println("Warning: Failed to set executable permission on " + tempFile.getAbsolutePath());
        }

        try (java.io.FileOutputStream os = new java.io.FileOutputStream(tempFile)) {
            byte[] buffer = new byte[4096];
            int read;
            while ((read = is.read(buffer)) != -1) {
                os.write(buffer, 0, read);
            }
        }

        flowExecutable = tempFile;
        return flowExecutable;
    }

    /**
     * Compile Flow source code to Java bytecode and load it as a module
     * @param source The Flow source code
     * @param moduleName The name to assign to the module
     * @return The loaded Module
     */
    public static Module compileAndLoad(String source, String moduleName) throws java.io.IOException, InterruptedException {
        if (!initialized) {
            throw new IllegalStateException("Flow runtime not initialized. Call FlowRuntime.init() first");
        }

        // Create temp source file
        java.io.File tempSource = java.io.File.createTempFile(moduleName, ".flow");
        tempSource.deleteOnExit();
        try (java.io.FileWriter writer = new java.io.FileWriter(tempSource)) {
            writer.write(source);
        }

        // Create temp output file (JAR)
        java.io.File tempDir = java.nio.file.Files.createTempDirectory("flow_compile_" + moduleName).toFile();
        tempDir.deleteOnExit();
        
        java.io.File outputFile = new java.io.File(tempDir, moduleName + ".jar");
        
        // Run transpiler
        java.io.File flowExe = getFlowExecutable();
        java.util.List<String> command = new java.util.ArrayList<>();
        command.add(flowExe.getAbsolutePath());
        command.add("transpile");
        command.add(tempSource.getAbsolutePath());
        command.add("--target");
        command.add("java");
        command.add("--output");
        command.add(outputFile.getAbsolutePath());
        command.add("--class");
        command.add(moduleName); // Ensure class name matches module name

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.inheritIO();
        Process process = pb.start();
        int exitCode = process.waitFor();

        if (exitCode != 0) {
            throw new RuntimeException("Flow compilation failed with exit code " + exitCode);
        }

        // Add JAR to search path
        moduleRegistry.addSearchPath(outputFile.getAbsolutePath());

        // Load the module
        // Note: The transpiler generates a class with the name specified by --class
        // ModuleRegistry expects the module name to match the class name (capitalized)
        // So if we pass "MyModule", it expects "MyModule" class.
        return moduleRegistry.load(moduleName);
    }

    /**
     * Execute a Flow script using the bundled CLI
     * @param scriptFile The Flow script file to run
     * @param args Arguments to pass to the script
     * @return The exit code of the process
     */
    public static int runScript(java.io.File scriptFile, String... args) throws java.io.IOException, InterruptedException {
        java.io.File flowExe = getFlowExecutable();
        
        java.util.List<String> command = new java.util.ArrayList<>();
        command.add(flowExe.getAbsolutePath());
        command.add("run");
        command.add(scriptFile.getAbsolutePath());
        for (String arg : args) {
            command.add(arg);
        }
        
        ProcessBuilder pb = new ProcessBuilder(command);
        pb.inheritIO();
        Process process = pb.start();
        return process.waitFor();
    }
}
