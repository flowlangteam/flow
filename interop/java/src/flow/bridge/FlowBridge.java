package flow.bridge;

/**
 * JNI Bridge for calling native Flow code from Java
 * 
 * This class provides native method declarations for:
 * - Calling Flow JIT-compiled functions
 * - Memory management between Java and Flow
 * - Type conversion utilities
 */
public class FlowBridge {
    
    static {
        try {
            System.loadLibrary("flow_jni");
        } catch (UnsatisfiedLinkError e) {
            // Try to load from JAR
            try {
                loadNativeLibraryFromJar();
            } catch (Exception ex) {
                System.err.println("Warning: flow_jni native library not found in java.library.path or classpath");
                System.err.println("Native Flow interop will not be available: " + ex.getMessage());
            }
        }
    }

    private static void loadNativeLibraryFromJar() throws java.io.IOException {
        String osName = System.getProperty("os.name").toLowerCase();
        String osArch = System.getProperty("os.arch").toLowerCase();
        
        String libName = "flow_jni";
        String extension;
        String prefix = "lib";
        
        if (osName.contains("win")) {
            extension = ".dll";
            prefix = "";
        } else if (osName.contains("mac")) {
            extension = ".dylib";
        } else {
            extension = ".so";
        }
        
        String resourcePath = "/native/" + libName + extension;
        java.io.InputStream is = FlowBridge.class.getResourceAsStream(resourcePath);
        
        if (is == null) {
            throw new java.io.FileNotFoundException("Native library not found in JAR at " + resourcePath);
        }
        
        java.io.File tempFile = java.io.File.createTempFile(prefix + libName, extension);
        tempFile.deleteOnExit();
        
        try (java.io.FileOutputStream os = new java.io.FileOutputStream(tempFile)) {
            byte[] buffer = new byte[4096];
            int read;
            while ((read = is.read(buffer)) != -1) {
                os.write(buffer, 0, read);
            }
        }
        
        System.load(tempFile.getAbsolutePath());
    }
    
    /**
     * Initialize the native Flow runtime
     */
    public static native void initNativeRuntime();
    
    /**
     * Shutdown the native Flow runtime
     */
    public static native void shutdownNativeRuntime();
    
    /**
     * Compile Flow source code to native code via JIT
     * @param sourceCode The Flow source code
     * @return Handle to the compiled module
     */
    public static native long compileFlow(String sourceCode);
    
    /**
     * Call a native Flow function
     * @param moduleHandle Handle from compileFlow
     * @param functionName Name of the function to call
     * @param args Arguments to pass
     * @return Result from the function
     */
    public static native Object callNativeFunction(long moduleHandle, String functionName, Object[] args);
    
    /**
     * Free a compiled module
     * @param moduleHandle Handle to free
     */
    public static native void freeModule(long moduleHandle);
    
    /**
     * Get version of the native runtime
     */
    public static native String getNativeVersion();
    
    /**
     * Check if native runtime is available
     */
    public static boolean isNativeAvailable() {
        try {
            getNativeVersion();
            return true;
        } catch (UnsatisfiedLinkError e) {
            return false;
        }
    }
}
