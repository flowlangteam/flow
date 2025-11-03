# Flow ↔ Java Interop

Bidirectional interoperability between Flow and Java.

## Features

### 1. Call Java from Flow
- Import and use Java classes and methods
- JNI bindings for native Java libraries
- Direct bytecode invocation for Flow-generated classes

### 2. Call Flow from Java
- Export Flow functions as Java-callable methods
- Generate Java wrapper classes
- Maven/Gradle integration

### 3. Compile Flow to Java Bytecode
- Generate `.class` files compatible with any JVM
- Use Flow code in existing Java projects
- No runtime dependencies

## Directory Structure

```
java/
├── src/
│   ├── FlowRuntime.java       # Runtime support for Flow code
│   ├── FlowBridge.java        # JNI bridge for native calls
│   └── examples/              # Example integrations
├── lib/
│   └── flow-runtime.jar       # Packaged runtime library
├── build.gradle               # Gradle build configuration
├── pom.xml                    # Maven configuration
└── README.md                  # This file
```

## Usage Examples

### Calling Java from Flow

```flow
// Import Java classes
extern "java" {
    class System {
        static fn println(s: String)
    }
    
    class ArrayList<T> {
        fn new() -> ArrayList<T>
        fn add(item: T) -> Bool
        fn get(index: I64) -> T
    }
}

// Use Java code in Flow
pub fn main() {
    let list = ArrayList::new()
    list.add("Hello")
    list.add("World")
    
    System::println(list.get(0))
}
```

### Calling Flow from Java

```java
// Generated wrapper for Flow functions
import flow.Runtime;
import flow.Module;

public class Main {
    public static void main(String[] args) {
        // Initialize Flow runtime
        Runtime.init();
        
        // Load Flow module
        Module math = Runtime.loadModule("math");
        
        // Call Flow function
        long result = (Long) math.call("fibonacci", 10);
        System.out.println("Result: " + result);
    }
}
```

### Direct Bytecode Integration

```bash
# Compile Flow to Java bytecode
flow transpile math.flow --target java --class Math

# Use in Java project (no Flow runtime needed)
javac -cp Math.class MyApp.java
java -cp .:Math.class MyApp
```

## Building

### Gradle
```bash
./gradlew build
```

### Maven
```bash
mvn clean install
```

## Installation

Add to your `build.gradle`:
```groovy
dependencies {
    implementation files('libs/flow-runtime.jar')
}
```

Or `pom.xml`:
```xml
<dependency>
    <groupId>flow.lang</groupId>
    <artifactId>flow-runtime</artifactId>
    <version>0.1.0</version>
</dependency>
```
