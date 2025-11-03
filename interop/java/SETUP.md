# Flow-Java Interop Setup Guide

This guide covers the complete setup and usage of Flow-Java bidirectional interoperability.

## 🎯 What's Included

### Runtime Components
1. **FlowRuntime.java** - Initialize Flow runtime, load modules
2. **Module.java** - Wrapper for calling Flow functions from Java
3. **ModuleRegistry.java** - Automatic module discovery and caching
4. **FlowBridge.java** - JNI bridge for native Flow code execution

### Build Artifacts
- `lib/flow-runtime.jar` - Java runtime classes
- `lib/libflow_jni.dylib` - Native JNI library (macOS)
- `lib/libflow_jni.so` - Native JNI library (Linux)  
- `lib/flow_jni.dll` - Native JNI library (Windows)

## 📋 Prerequisites

- **JDK 11+** (for Java compilation)
- **Rust toolchain** (for building JNI library)
- **Gradle** or **Maven** (for building Java components)
- **Flow CLI** (for transpiling Flow code)

## 🔨 Building

### Option 1: Gradle (Recommended)

```bash
./build.sh
```

### Option 2: Maven

```bash
./build.sh maven
```

### Manual Build Steps

```bash
# 1. Build JNI library (Rust/Cranelift)
cd ../../flow_c
cargo build -p flow_jni --release

# 2. Copy native library
cp target/release/libflow_jni.dylib ../interop/java/lib/

# 3. Build Java runtime
cd ../interop/java
gradle buildRuntime
```

## 🚀 Usage

### Approach 1: Bytecode Transpilation

**Transpile Flow to JVM bytecode:**

```bash
# Create Flow code
cat > math.flow << 'EOF'
func add(a: Int, b: Int) -> Int {
    a + b
}

func multiply(a: Int, b: Int) -> Int {
    a * b
}
EOF

# Transpile to Java bytecode
flow transpile --target java math.flow

# Generates: Math.class
```

**Call from Java:**

```java
import flow.runtime.*;

public class Main {
    public static void main(String[] args) {
        // Initialize Flow runtime
        FlowRuntime.init();
        
        // Load Flow module
        Module math = FlowRuntime.loadModule("Math");
        
        // Call Flow functions
        long result = math.callLong("add", 5L, 3L);
        System.out.println("5 + 3 = " + result);  // Output: 8
        
        result = math.callLong("multiply", 4L, 7L);
        System.out.println("4 * 7 = " + result);  // Output: 28
    }
}
```

### Approach 2: JIT Compilation via JNI

**Compile Flow code at runtime:**

```java
import flow.bridge.*;

public class JitExample {
    public static void main(String[] args) {
        // Initialize JNI bridge
        FlowBridge.initializeRuntime();
        
        // Flow source code
        String flowSource = """
            func factorial(n: Int) -> Int {
                if n <= 1 {
                    1
                } else {
                    n * factorial(n - 1)
                }
            }
        """;
        
        // JIT compile to native code
        long moduleId = FlowBridge.compileFlow(flowSource, "factorial_module");
        
        // Call the native function
        double result = FlowBridge.callNativeFunction(moduleId, "factorial", null);
        System.out.println("factorial(5) = " + (long)result);  // Output: 120
        
        // Cleanup
        FlowBridge.unloadModule(moduleId);
        FlowBridge.shutdownRuntime();
    }
}
```

## 📚 API Reference

### FlowRuntime

```java
// Initialize the Flow runtime
FlowRuntime.init();

// Load a transpiled Flow module
Module mod = FlowRuntime.loadModule("ClassName");

// Discover modules on classpath
List<String> modules = ModuleRegistry.discoverModules();

// Get cached module
Module cached = ModuleRegistry.getModule("ClassName");
```

### Module

```java
// Generic function call (returns Object)
Object result = module.call("functionName", arg1, arg2);

// Typed calls
long longResult = module.callLong("functionName", args...);
int intResult = module.callInt("functionName", args...);
boolean boolResult = module.callBoolean("functionName", args...);
String strResult = module.callString("functionName", args...);

// Get function names
List<String> functions = module.getFunctions();
```

### FlowBridge (JNI)

```java
// Initialize native runtime
FlowBridge.initializeRuntime();

// JIT compile Flow source
long moduleId = FlowBridge.compileFlow(sourceCode, moduleName);

// Call compiled function
double result = FlowBridge.callNativeFunction(moduleId, "funcName", args);

// Get function list
String functions = FlowBridge.getFunctions(moduleId);  // "func1,func2,func3"

// Unload module
FlowBridge.unloadModule(moduleId);

// Shutdown runtime
FlowBridge.shutdownRuntime();
```

## 🔧 Integration

### Gradle

```gradle
dependencies {
    implementation files('lib/flow-runtime.jar')
}

// Ensure native library is on java.library.path
run {
    systemProperty 'java.library.path', file('lib').absolutePath
}
```

### Maven

```xml
<dependency>
    <groupId>flow.lang</groupId>
    <artifactId>flow-runtime</artifactId>
    <version>0.1.0</version>
    <scope>system</scope>
    <systemPath>${project.basedir}/lib/flow-runtime.jar</systemPath>
</dependency>
```

## 🐛 Troubleshooting

### UnsatisfiedLinkError

```
java.lang.UnsatisfiedLinkError: no flow_jni in java.library.path
```

**Solution:** Set the library path when running:

```bash
java -Djava.library.path=lib -jar your-app.jar
```

### ClassNotFoundException

```
java.lang.ClassNotFoundException: YourFlowClass
```

**Solution:** Ensure the transpiled .class file is on the classpath:

```bash
java -cp .:lib/flow-runtime.jar Main
```

### Module Not Found

**Solution:** Check module is in the classpath:

```java
List<String> modules = ModuleRegistry.discoverModules();
System.out.println("Available modules: " + modules);
```

## 📝 Examples

Run the included examples:

```bash
# Bytecode transpilation example
gradle runCallFlowExample

# JIT compilation example
gradle runNativeFlowExample

# Or with Maven
mvn exec:java -Dexec.mainClass=flow.examples.CallFlowExample
```

## 🏗️ Architecture

```
Flow Code (.flow)
     │
     ├─→ [Transpiler] ─→ JVM Bytecode (.class)
     │                        │
     │                        └─→ Java Runtime (Module.call)
     │
     └─→ [JIT Compiler] ─→ Native Code
                               │
                               └─→ JNI Bridge (FlowBridge.callNativeFunction)
```

## 🔐 Performance Characteristics

| Approach      | Startup Time | Execution Speed | Memory Overhead | Use Case                |
|---------------|--------------|-----------------|-----------------|-------------------------|
| Bytecode      | Fast         | Good            | Low             | JVM-only environments   |
| JIT + JNI     | Slow         | Excellent       | Higher          | Performance-critical    |

## 🎓 Advanced Topics

### Type Mapping

| Flow Type | Java Type (Bytecode) | JNI Type    |
|-----------|---------------------|-------------|
| Int       | long                | jlong       |
| Float     | double              | jdouble     |
| Bool      | boolean             | jboolean    |
| String    | String              | jstring     |

### Custom Class Loading

```java
// Load from custom location
URL url = new File("/path/to/classes").toURI().toURL();
URLClassLoader loader = new URLClassLoader(new URL[]{url});
Class<?> flowClass = loader.loadClass("MyFlowModule");
```

### Multi-threading

Both approaches are thread-safe when using separate Module/module instances per thread. For concurrent access to the same module:

```java
Module module = FlowRuntime.loadModule("Shared");
// Safe: multiple threads calling different functions
executor.submit(() -> module.callLong("func1", 10));
executor.submit(() -> module.callLong("func2", 20));
```

## 📄 License

MIT License - See main Flow repository for details.
