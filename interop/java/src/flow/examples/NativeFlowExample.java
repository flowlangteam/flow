package flow.examples;

import flow.bridge.FlowBridge;

/**
 * Example: Using native Flow JIT compilation from Java
 * 
 * This demonstrates calling Flow's JIT compiler from Java
 * to compile and execute Flow code at runtime.
 */
public class NativeFlowExample {
    public static void main(String[] args) {
        System.out.println("=== Native Flow JIT Example ===\n");
        
        // Check if native runtime is available
        if (!FlowBridge.isNativeAvailable()) {
            System.err.println("Native Flow runtime not available");
            System.err.println("Make sure flow_jni library is in java.library.path");
            return;
        }
        
        // Initialize native runtime
        FlowBridge.initNativeRuntime();
        System.out.println("Native Runtime Version: " + FlowBridge.getNativeVersion());
        
        try {
            // Flow source code to compile
            String flowCode = 
                "pub fn calculate(x: I64) -> I64 {\n" +
                "    x * 2 + 10\n" +
                "}\n" +
                "\n" +
                "pub fn fibonacci(n: I64) -> I64 {\n" +
                "    if n <= 1 {\n" +
                "        n\n" +
                "    } else {\n" +
                "        fibonacci(n - 1) + fibonacci(n - 2)\n" +
                "    }\n" +
                "}";
            
            System.out.println("\nCompiling Flow code...");
            long moduleHandle = FlowBridge.compileFlow(flowCode);
            System.out.println("✓ Compilation successful (handle: " + moduleHandle + ")");
            
            // Call compiled functions
            System.out.println("\n--- Calling Compiled Functions ---");
            
            Object result1 = FlowBridge.callNativeFunction(moduleHandle, "calculate", new Object[]{5L});
            System.out.println("calculate(5) = " + result1);
            
            Object result2 = FlowBridge.callNativeFunction(moduleHandle, "fibonacci", new Object[]{10L});
            System.out.println("fibonacci(10) = " + result2);
            
            // Cleanup
            FlowBridge.freeModule(moduleHandle);
            System.out.println("\n✓ Module freed");
            
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            FlowBridge.shutdownNativeRuntime();
        }
    }
}
