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
        // Load native library
        // This would be the compiled Flow runtime with JNI bindings
        try {
            System.loadLibrary("flow_jni");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Warning: flow_jni native library not found");
            System.err.println("Native Flow interop will not be available");
        }
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
