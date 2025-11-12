# Flow Compiler TODO Tracker

This file aggregates the outstanding `TODO` comments that still exist in the Flow compiler workspace. Counts come from a direct search of `flow_c/**` on November 12, 2025.

## Summary

- **Total TODO markers**: 38 across 6 subsystems
- **Status updates since last revision**:
  - `flow_codegen` crate was removed, so its previous TODOs no longer apply.
  - Analyzer TODOs for struct handling, higher-order calls, and type compatibility were completed.
- **Key themes**: struct layout & linking work in the Cranelift pipeline, missing transpiler backends, richer IDE/JNI support, and generics in the AST.

---

## Cranelift Compiler Pipeline (`flow_c/flow_compiler`)

- `src/jit_engine.rs`: Implement proper struct type handling inside the JIT lowering path.
- `src/aot_wrapper.rs`: Multiple gaps remain—propagate optimisation levels, convert JIT output to object files, compute real struct size/alignment, link multiple modules, and finalise the JIT-to-object hand-off.
- `src/java_wrapper.rs`: Enable debug info, translate Flow types to JVM descriptors, generate full JAR/multi-class output, and add Java-specific validation passes.
- `src/aot_wrapper_old.rs`: Legacy wrapper still contains TODOs for optimisation flags, struct layout math, linker integration, output configuration, and validation.
- `src/jit_wrapper.rs`: Similar legacy TODOs covering optimisation, layout calculation, and validation.

## Core Language Representation (`flow_c/flow_ast`)

- `src/lib.rs`: Implement real generic type support (type variables + constraints) beyond the current placeholders.

## Command-Line Interface (`flow_c/flow_cli`)

- `src/module_manager.rs`: Populate the compiled function pointer table and exported type metadata when loading modules.
- `src/main.rs`: CLI subcommands still stub out Python, JavaScript, C, Rust, and WebAssembly transpiler backends.

## IDE / Language Server (`flow_c/flow_lsp`)

- `src/handlers.rs`: Implement AST-backed go-to-definition and accurate span->position mapping instead of the current placeholders.

## Java Interop (`flow_c/flow_jni`)

- `src/lib.rs`: Complete argument marshalling for all supported types and allow bridging functions with parameters (current bridge assumes no-arg calls).

## Transpiler Backends (`flow_c/flow_transpiler`)

- `flow_transpiler_java/src/lib.rs` & `constant_pool.rs`: Replace placeholder bytecode offsets and simplified constant-pool handling with precise implementations.

## Outstanding Work Not Yet Categorised

- `flow_c/flow_compiler/src/java_wrapper.rs`: Generate JVM type descriptors from Flow types.
- `flow_c/flow_compiler/src/lib.rs` and related modules: ensure configuration options propagate through every backend (not yet tracked by an inline TODO but required to close several of the above items).

---

## Suggested Next Steps

1. Prioritise the Cranelift struct layout/linking TODO cluster—it blocks both AOT packaging and accurate struct semantics across the toolchain.
2. Decide which transpiler backends (if any) should be implemented next; remove stubs that are no longer on the roadmap.
3. Add regression tests for each resolved TODO so progress is captured automatically.
4. Keep this tracker aligned with live code by re-running `rg "TODO" flow_c` after significant refactors.
