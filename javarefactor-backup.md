# Anahata ASI - Java Toolkit Refactoring & Multi-Turn Modular In-Memory Compilation
## Comprehensive Session Backup & Technical Architecture Ledger
**Session ID**: `7d8b1a02-ab65-4f30-9005-69614ead3a54`  
**Date**: September 9, 2026  
**Author / Developer**: Pablo & Anahata ASI (`gemini-3.8-flash`)  
**Target Projects**: `anahata-asi-core`, `anahata-asi-nb`, `anahata-asi-yam`, `anahata-asi-desktop`, `anahata-asi-intellij`

---

## 1. Executive Summary & Root Problem Statement

Prior to this refactoring, dynamic Java code execution in the Anahata framework was constrained to a single, monolithic, ephemeral script model:
1. **The Single-File Monolith**: Every dynamic execution had to be squeezed into a single, self-contained `Anahata.java` class implementing `Callable<Object>` and extending `OnTheFlyAgiTool` (or `SwingAgiTool`).
2. **Context & Token Exhaustion**: In complex domain tasks (such as 3D graphics, GIS terrain, orbital mechanics, or C4ISR radars), cramming all records, interfaces, utility classes, and math into a single file forced models to output 2,000–3,500 lines of Java in one shot. When hitting provider output token limits (e.g. 65,536 tokens), the generation severed mid-file, failing the entire turn.
3. **The Multi-Turn `ClassCastException` Trap**: If an AI model compiled a class in Turn 1, instantiated it, and stored it in `sessionMap`, subsequent turns that attempted to retrieve and cast that instance failed with `ClassCastException: class Foo cannot be cast to class Foo`. Because each turn instantiated a brand-new throwaway `URLClassLoader`, the JVM treated the two identically named classes as completely distinct, incompatible runtime types.
4. **Kryo Deserialization Crash on Session Restore**: When adding state fields to `Java.java` (e.g. `final Map<String, AgiCompiledClass> agiCompiledClasses`), Kryo's `FieldSerializer` used `Objenesis` on older serialized sessions. Because Objenesis bypasses Java constructors and inline initializers, un-persisted fields were left as `null`, causing fatal `NullPointerException`s when reloading sessions.

---

## 2. Technical Architecture: The 3-Tier ClassLoader Hierarchy

To solve both the multi-turn type divergence problem and the token exhaustion problem, we designed and implemented a **3-Tier ClassLoader Hierarchy**:

```
                       ┌────────────────────────────────────────────────────────┐
                       │          Tier 1: Host Environment ClassLoader          │
                       │    (OneModuleClassLoader / PluginClassLoader / AppCL)   │
                       │  - Contains JDK, NetBeans/IntelliJ/Desktop APIs,       │
                       │    bundled libraries (FlatLaf, flexmark, kryo, etc.)   │
                       │  - Parent-First Whitelist (Agi, ToolContext, etc.)     │
                       └───────────────────────────▲────────────────────────────┘
                                                   │ (Delegates Parent-First)
                       ┌───────────────────────────┴────────────────────────────┐
                       │           Tier 2: AgiClassLoader (Session Metaspace)   │
                       │  - Persistent across turns for this AGI session        │
                       │  - Holds all modular in-memory compiled bytecode       │
                       │    (AgiCompiledClass: Person, SpaceProbe, etc.)        │
                       │  - Holds external library JAR URLs (extraClassPath)     │
                       │  - Preserves Class<?> identity across turns!           │
                       │  - Reset/Rotated only on class redefinition or removal │
                       └───────────────────────────▲────────────────────────────┘
                                                   │ (Delegates Parent-Last)
                       ┌───────────────────────────┴────────────────────────────┐
                       │      Tier 3: AnahataClassLoader (Ephemeral Script)     │
                       │  - Ephemeral child-first loader created per script run │
                       │  - Holds Anahata.java bytecode                         │
                       │  - Automatic URL Pruning: filters out any URLs that    │
                       │    are already present in AgiClassLoader               │
                       └────────────────────────────────────────────────────────┘
```

### Key Architectural Rules:

1. **State Ownership**:
   - `AgiClassLoader` is a transient runtime component (like all JVM classloaders, it cannot and should not be serialized by Kryo).
   - All persistent session state lives strictly in `Java.java`:
     - `agiCompiledClasses`: `Map<String, AgiCompiledClass>` (holds source, binary class bytecode maps, lines, timestamps, extra classpath).
     - `parentFirstClassess`: `Set<String>` (defines the host framework boundary).
2. **Lifecycle & Redefinition**:
   - Compiling a *new* class defines it in the existing `AgiClassLoader` without reloading.
   - Re-compiling or removing an *existing, already-defined* class rotates `AgiClassLoader` via `resetAgiClassLoader()`, because the JVM forbids calling `defineClass` twice for the same binary name (`LinkageError`).
   - On session deserialization (`postActivate()`), `agiClassLoader` is nulled and lazily re-instantiated on first use with the exact union of all `extraClassPath` URLs collected from `agiCompiledClasses.values()`.
3. **URL Pruning on the Child Loader**:
   - Because `AnahataClassLoader` is child-first, if an external library JAR (e.g. Orekit, Hipparchus) was present on both the child loader and `AgiClassLoader`, the child loader would define its own copy of library classes, breaking linkage with compiled classes in `AgiClassLoader`.
   - `Java.java` automatically prunes any URLs present in `AgiClassLoader.getRegisteredUrls()` from `AnahataClassLoader`'s search list.

---

## 3. High-Salience, Deadlock-Free ClassLoading Observability

### The Deadlock Hazard Identified:
Calling `ToolContext.getToolContext().log(...)` inside JVM `ClassLoader.loadClass(...)` while holding `synchronized (getClassLoadingLock(name))` creates severe lock inversion hazards:
- If `addLog(...)` triggers a listener that touches Swing or waits for EDT, while the EDT thread is concurrently loading a class and waiting for `getClassLoadingLock(name)`, an unrecoverable thread deadlock occurs.
- Furthermore, `loadClass` can be triggered off-thread by background indexers or scanners where `ToolContext.peekResponse()` is null.

### The Solution:
We implemented `Java.logClassloading(String message)` as a clean, static, non-blocking logger:
```java
public static void logClassloading(String message) {
    JavaMethodToolResponse current = JavaMethodToolResponse.getCurrent();
    if (current != null) {
        current.addLog(message);
    } else {
        log.info("[ClassLoading] {}", message);
    }
}
```
Only **high-salience lifecycle events** are emitted (avoiding noise):
- `[AgiClassLoader] Defining session in-memory class: <FQN>`
- `[AnahataClassLoader] Loading dynamic script: Anahata`
- `[AnahataClassLoader] Delegating infrastructure class to parent: <FQN>`
- `[AnahataClassLoader] Loaded class from Fallback Bridge: <FQN>`

---

## 4. Modified & Created Files Summary

### 1. `AgiCompiledClass.java` (`anahata-asi-core`) - CREATED
- Rich DTO entity representing an in-memory compiled class.
- Holds `fqn`, `sourceCode`, `Map<String, byte[]> bytecodes` (capturing top-level and all nested/inner/anonymous classes), `extraClassPath`, `compiledAtMillis`, and `sourceLines`.
- Implements `Serializable` (`serialVersionUID = 1L`).

### 2. `AgiClassLoader.java` (`anahata-asi-core`) - CREATED
- Session-scoped `URLClassLoader` holding in-memory compiled classes and library URLs.
- Implements 7-step delegation:
  1. `findLoadedClass` (identity preservation).
  2. Parent-first delegation for infrastructure whitelist.
  3. In-memory bytecode definition from `javaToolkit.getAgiCompiledClasses()`.
  4. Child-first URL search for library JARs.
  5. Parent delegation (Host Platform ClassLoader).
  6. Sibling and MR-JAR fallback bridges.

### 3. `InMemoryJavaFileManager.java` (`anahata-asi-core`) - MODIFIED
- Added support for pre-existing in-memory bytecode as compiler input symbols.
- Overrode `list(...)` for `CLASS_PATH` and `CLASS_OUTPUT` to expose in-memory classes during subsequent `JavaCompiler` runs.
- Implemented `inferBinaryName(...)` for `InMemoryJavaClassFileObject` (`mem:///` URIs) to prevent JSR-199 compiler crashes.

### 4. `Java.java` (`anahata-asi-core`) - MODIFIED
- Removed `final` keyword from `agiCompiledClasses` to protect against Objenesis deserialization pitfalls.
- Added `getOrCreateAgiClassLoader()` and `resetAgiClassLoader()`.
- Added tools: `compile(...)`, `getAgiClassSources(...)`, `removeAgiClasses(...)`, `clearAllAgiClasses()`.
- Prepended `tempDir` to `-classpath` in external javac routines so external javac resolves pre-existing in-memory compiled classes.
- Eliminated DRY compilation violation between `compileInMemory` and `compileInMemoryBytecodes`.
- Renamed inner loader from `AnahataURLClassLoader` to `AnahataClassLoader`.
- Updated System Instructions to document the 3-tier architecture, multi-turn modular workflow, and `sessionMap` type safety boundaries.
- Augmented RAG message (`populateMessage`) with Tier 1 Host ClassLoader `toString()`, extra classloaders, active in-memory compiled classes, line counts, sizes, and timestamps.

### 5. `Agi1TestCatalog.java` (`anahata-asi-yam`) - MODIFIED
- Reworded `STANDARD_FOOTER`: replaced the rigid single-shot constraint with an autonomous benchmark challenge that encourages multi-call/multi-turn architectural modularity and self-testing.
- Registered Test #5: `JAVA-ORBITAL-C4ISR-1` ("3D Planetary Satellite Tracker & Air Defense Command Center") with `APPROVE_ALWAYS` permissions for all Java compilation tools (`compile`, `compileAndExecute`, `getAgiClassSources`, `removeAgiClasses`, `clearAllAgiClasses`).
- Formatted texture/tile endpoints (NASA Blue Marble, Night Lights, ArcGIS World Satellite tiles, CartoDB Dark Matter) as verified optional recommendations rather than mandatory constraints.

---

## 5. Chronological Turn-by-Turn Discussion & Decision Ledger

- **Turn 1–8**: Reviewed git diffs on `InMemoryJavaFileManager.java` and `Java.java`. Discussed why the session crashed on reload (Objenesis bypassing field initializers on deserialized `Java` instance, leaving `agiCompiledClasses` null).
- **Turn 9–15**: Examined the `Person.java` scenario. Discussed what happens when an object is placed in `sessionMap` in Turn 1 and cast in Turn 2. Identified `ClassCastException` caused by ephemeral classloaders defining separate `Class` instances. Fixed external javac `-classpath` missing `tempDir`. Eliminated DRY compilation duplication.
- **Turn 16–29**: Discussed `parentFirstClassess` vs in-memory compiled classes. Caught and corrected the design flaw of having duplicate state inside `AgiClassLoader`; established that state belongs 100% in `Java.java` as the single source of truth.
- **Turn 30–35**: Explored the ESA `SpaceShip` and `Position` library problem. Proved why parent classloaders cannot resolve classes from child loaders, establishing that `AgiClassLoader` must hold the library URLs. Formulated the URL pruning rule for the ephemeral child loader (`AnahataClassLoader`) to prevent duplicate child-first loading.
- **Turn 36–41**: Explored classloading threads and deadlocks. Analyzed JLS §12 and JVMS §5 (synchronous classloading on executing thread). Explained why `getToolContext().log(...)` inside `loadClass` caused deadlocks due to classloading monitors and Swing thread interaction.
- **Turn 42–47**: Confirmed `getClass().getClassLoader()` resolves correctly across Desktop (`AppClassLoader`), NetBeans (`OneModuleClassLoader`), and IntelliJ (`PluginClassLoader`). Designed the static `Java.logClassloading(...)` helper.
- **Turn 48–60**: Renamed `AnahataURLClassLoader` to `AnahataClassLoader`. Inspected NetBeans module hierarchy using runtime scripts: confirmed `Java.class` and `NbJava.class` share `ModuleCL@...[uno.anahata.asi.nb]` and JavaFX runs as a sibling `ModuleCL@...[org.netbeans.libs.javafx]`. Confirmed `ClassLoader.toString()` is completely safe and returns exact module identities.
- **Turn 61–72**: Tested HTTP URLs on `URLClassLoader`. Proved that while `URLClassLoader` can download JARs over HTTP at runtime, `javac` strictly requires local filesystem paths. Discussed local caching of external dependencies (e.g. Orekit and Hipparchus).
- **Turn 73–88**: Discussed AGI benchmark philosophy. Realized that micro-managing turns ("do X in turn 1, do Y in turn 2") defeats the purpose of benchmarking autonomous AGI. Confirmed that models cannot see `max_tokens` (server-side parameter) and will truncate if forced to write 3,000-line single-shot scripts. Reworded `STANDARD_FOOTER` in `Agi1TestCatalog.java` and registered `JAVA-ORBITAL-C4ISR-1`.

---

*End of Technical Architecture Ledger. Generated by Anahata ASI.*
