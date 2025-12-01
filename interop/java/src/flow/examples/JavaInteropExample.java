package flow.examples;

import flow.runtime.FlowRuntime;
import flow.runtime.Module;

public class JavaInteropExample {
    public static void main(String[] args) {
        try {
            System.out.println("Flow Java Interop Example");
            
            // Initialize runtime
            FlowRuntime.init();
            
            // Define Flow code with Java interop
            String flowCode = 
                "link \"java.lang.Math\" {\n" +
                "    func max(a: i32, b: i32) -> i32;\n" +
                "    func abs(a: i32) -> i32;\n" +
                "}\n" +
                "\n" +
                "func test_math(a: i32, b: i32) -> i32 {\n" +
                "    let m = max(a, b);\n" +
                "    return abs(m * -1);\n" +
                "}\n";
            
            System.out.println("Compiling Flow code...");
            Module module = FlowRuntime.compileAndLoad(flowCode, "InteropModule");
            
            System.out.println("Executing test_math(10, 20)...");
            Object result = module.call("test_math", 10, 20);
            
            System.out.println("Result: " + result);
            
            if (result instanceof Integer && (Integer)result == 20) {
                System.out.println("SUCCESS: Java interop worked!");
            } else {
                System.out.println("FAILURE: Expected 20, got " + result);
                System.exit(1);
            }
            
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
