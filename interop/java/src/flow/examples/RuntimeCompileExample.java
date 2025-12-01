package flow.examples;

import flow.runtime.FlowRuntime;
import flow.runtime.Module;

public class RuntimeCompileExample {
    public static void main(String[] args) {
        try {
            // Initialize runtime
            FlowRuntime.init();
            
            System.out.println("Compiling and loading Flow code at runtime...");
            
            String source = 
                "func add(a: i64, b: i64) -> i64 {\n" +
                "    return a + b;\n" +
                "}\n" +
                "\n" +
                "func greet(name: String) -> String {\n" +
                "    return \"Hello, \" + name + \"!\";\n" +
                "}\n";
            
            // Compile and load
            Module module = FlowRuntime.compileAndLoad(source, "RuntimeModule");
            
            System.out.println("Module loaded: " + module.getName());
            
            // Call functions
            // Note: Flow i64 maps to Java long
            long sum = module.callLong("add", 10L, 20L);
            System.out.println("add(10, 20) = " + sum);
            
            String greeting = module.callString("greet", "World");
            System.out.println("greet(\"World\") = " + greeting);
            
            if (sum == 30L && "Hello, World!".equals(greeting)) {
                System.out.println("SUCCESS: Runtime compilation and execution worked!");
            } else {
                System.err.println("FAILURE: Unexpected results");
                System.exit(1);
            }
            
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        } finally {
            FlowRuntime.shutdown();
        }
    }
}
