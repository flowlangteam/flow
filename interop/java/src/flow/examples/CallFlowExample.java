package flow.examples;

import flow.runtime.FlowRuntime;
import flow.runtime.Module;

/**
 * Example: Calling Flow functions from Java
 * 
 * Prerequisites:
 * 1. Compile a Flow file to bytecode:
 *    flow transpile math.flow --class Math
 * 
 * 2. Add Math.class to your classpath
 * 
 * 3. Run this example:
 *    javac -cp .:../flow-runtime.jar CallFlowExample.java
 *    java -cp .:../flow-runtime.jar:Math.class flow.examples.CallFlowExample
 */
public class CallFlowExample {
    public static void main(String[] args) {
        // Initialize Flow runtime
        System.out.println("=== Flow Interop Example ===\n");
        FlowRuntime.init();
        
        try {
            // Load a Flow module (assumes Math.class is in classpath)
            // This would be compiled from a Flow file like:
            //   pub fn add(a: I64, b: I64) -> I64 { a + b }
            //   pub fn fibonacci(n: I64) -> I64 { ... }
            
            System.out.println("Loading Flow module 'Math'...");
            Module math = FlowRuntime.loadModule("Math");
            
            // List available functions
            System.out.println("\nAvailable functions:");
            for (String funcName : math.listFunctions()) {
                System.out.println("  - " + funcName);
            }
            
            // Call Flow functions
            System.out.println("\n--- Calling Flow Functions ---");
            
            if (math.hasFunction("add")) {
                long result = math.callLong("add", 10L, 32L);
                System.out.println("add(10, 32) = " + result);
            }
            
            if (math.hasFunction("multiply")) {
                long result = math.callLong("multiply", 6L, 7L);
                System.out.println("multiply(6, 7) = " + result);
            }
            
            if (math.hasFunction("fibonacci")) {
                long result = math.callLong("fibonacci", 10L);
                System.out.println("fibonacci(10) = " + result);
            }
            
            System.out.println("\n✓ All Flow functions called successfully!");
            
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            // Cleanup
            FlowRuntime.shutdown();
        }
    }
}
