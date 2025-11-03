#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

BUILD_TOOL="${1:-gradle}"

echo "🔨 Building Flow Runtime for Java interop..."
echo "   Using: $BUILD_TOOL"
echo ""

case "$BUILD_TOOL" in
    gradle)
        if command -v gradle &> /dev/null; then
            echo "📦 Building with Gradle..."
            gradle buildRuntime
            echo ""
            echo "✅ Build complete!"
            echo "   JAR: lib/flow-runtime.jar"
        else
            echo "❌ Gradle not found. Install Gradle or use: ./build.sh maven"
            exit 1
        fi
        ;;
    
    maven|mvn)
        if command -v mvn &> /dev/null; then
            echo "📦 Building with Maven..."
            mvn clean package
            echo ""
            echo "✅ Build complete!"
            echo "   JAR: lib/flow-runtime-0.1.0.jar"
        else
            echo "❌ Maven not found. Install Maven or use: ./build.sh gradle"
            exit 1
        fi
        ;;
    
    *)
        echo "❌ Unknown build tool: $BUILD_TOOL"
        echo "   Usage: ./build.sh [gradle|maven]"
        exit 1
        ;;
esac

echo ""
echo "📚 To use the runtime in your Java project:"
echo ""
echo "   Gradle:"
echo "   implementation files('lib/flow-runtime.jar')"
echo ""
echo "   Maven:"
echo "   <dependency>"
echo "       <groupId>flow.lang</groupId>"
echo "       <artifactId>flow-runtime</artifactId>"
echo "       <version>0.1.0</version>"
echo "       <scope>system</scope>"
echo "       <systemPath>\${project.basedir}/lib/flow-runtime.jar</systemPath>"
echo "   </dependency>"
echo ""
echo "🚀 Run examples:"
echo "   gradle runCallFlowExample"
echo "   mvn exec:java -Dexec.mainClass=flow.examples.CallFlowExample"
