/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Maven Multi-Module Project

> [!DANGER]
> **CRITICAL CODING PRINCIPLE**
> **NEVER** attempt to write or refactor code without first loading the relevant Java types (classes, interfaces, etc.) into your context. Coding "blind" leads to hallucinations, compilation errors, and architectural drift. Always use `LocalFiles.readFile` or `JavaSources.getSource` to ensure you have the ground truth before proposing changes.

> [!DANGER]
> **NO HACKING OUR OWN CODE**
> We do not implement "dirty hacks" or workarounds to mask architectural flaws or API limitations in our own codebase. If a design is broken, we fix the design. We do not "hack" our way around it.

This document provides the high-level strategic overview for the entire `anahata-ai-parent` project.

> [!IMPORTANT]
> **BETA PROJECT / RUNTIME CLARIFICATION**
> This is a project under active development (V2). It is **NOT** the current runtime environment of the AI Assistant. 
> The current runtime is the **V1** architecture (`gemini-java-client` and `anahata-netbeans-ai`). 
> Do not expect the code in this project to be reflected in my current capabilities or internal state.

## 1. Grand Strategy: "The Flywheel"

The project's strategy is based on the "Flywheel" model, designed to build a powerful ecosystem around a core commercial engine.

-   **Engine (`anahata-asi`):** This is the core commercial framework, containing the model-agnostic logic, a rich domain model, and the powerful V2 tool framework. It is **dual-licensed** under the AGPLv3 (for open-source use) and the Anahata Software License (ASL) v108 (for commercial/private use).
-   **Showcase (`anahata-netbeans-ai`):** This is the free, open-source NetBeans plugin that serves as the primary showcase for the engine's capabilities. It drives community adoption, provides real-world validation, and demonstrates the power of deep IDE integration.

## 2. Daily Operations & Metrics
-   **High-ROI Ledger Update:** You MUST check the `ledger.md` file in the project root every day. 
-   **Download Tracking:** Visit the NetBeans Plugin Portal ([https://plugins.netbeans.apache.org/catalogue/?id=125](https://plugins.netbeans.apache.org/catalogue/?id=125)) at least once a day, fetch the current download count, and record it in the `ledger.md` milestone log. This is critical for tracking our "Deep Strike" distribution ROI.

## 3. Log of the Singularity (Recent Milestones)
-   **JDK 25 Standardization (Jan 2026):** Standardized the entire ecosystem (V1 and V2) on **JDK 25** for all builds and Javadoc generation, both locally and in GitHub Actions.
-   **JASI Portal Launch (Jan 2026):** Successfully launched the V2 portal at `asi.anahata.uno`. The site features a "Blaugrana-Noir" aesthetic and introduces the **Sextete of JASI**.
-   **Modern CI/CD Integration:** Transitioned to a direct GitHub Actions deployment workflow for the website and Javadocs, eliminating the need for a `gh-pages` branch.
-   **Sonatype Central Portal Success:** Configured the project for automated deployment to Sonatype Central Portal. Snapshots and releases are now correctly routed using the `sonatype-central` server ID.
-   **Yam Module Integration:** The `anahata-asi-yam` module is active and serves as the hub for "fun" agentic capabilities like the integrated radio and media tools.

## 4. Core Modules

The project is divided into the following active modules:

1.  **`anahata-asi-core`**: The foundational, model-agnostic framework. Contains all core interfaces, the domain model, and the tool-chain.
2.  **`anahata-asi-gemini`**: The first provider-specific implementation, acting as an Adapter between the Google Gemini API and the core framework.
3.  **`anahata-asi-swing`**: A reusable, provider-agnostic Swing UI component for building agentic workflows.
4.  **`anahata-asi-cli`**: The command-line interface for interacting with JASI.
5.  **`anahata-asi-standalone`**: A standalone Java application for running JASI outside of an IDE.
6.  **`anahata-asi-web`**: The official JASI Portal and documentation hub.
7.  **`anahata-asi-nb`**: The V2 NetBeans integration module.
8.  **`anahata-asi-yam`**: The "Yet Another Module" for creative and experimental agentic tools.

## 5. Strategic Documents

This project uses a set of key documents to guide development. For detailed information, please refer to the following:

-   **`v2-commanders-briefing.md`**: Located in the project root, this document contains the overall mission objectives, V2 migration plan, and high-priority tactical goals.
-   **`anahata-asi-core/anahata.md`**: Contains the detailed technical vision and architectural summary for the core framework module.
-   **`ci.md`**: Contains the CI/CD strategy, website deployment details, and Javadoc configuration notes.

## 6. Coding Principles (Applies to ALL Modules)

> [!CAUTION]
> **PARAMOUNT PRINCIPLES: SIMPLICITY AND STABILITY**
> The absolute priority for all development is **Simplicity and Stability** (or Stability through Simplicity). These principles rule above all others. 
> - **Core Discussion**: Any proposed changes to this module **MUST** be discussed and agreed upon with the user in the conversation before calling `suggestChange`.
> - **No Dirty Hacks**: Avoid "dirty hacks" or workarounds (e.g., `SwingUtilities.invokeLater` or `SwingUtilities.updateComponentTreeUI` to mask initialization order issues). If a design leads to race conditions or UI glitches, it requires a proper refactoring of the underlying architecture, not a patch for the symptoms.
> - **Unified Content API**: Always prefer `message.addTextPart(text)` or `message.addBlobPart(...)` over direct instantiation of `TextPart` or `BlobPart`. This ensures that the message can control the concrete part types and initialization order.
> - **No Redundant Signatures**: Avoid adding multiple methods with different signatures that perform the same logical operation. Keep the API lean and consistent.
> - **No Secondary Constructors**: Do not add "secondary" or "convenience" constructors to work around UI glitches or initialization order problems. Address the root cause in the primary constructor or the factory method.
> - **Identity & Distributed Observability**: Message metadata must clearly distinguish between the **Logical Actor** (`getFrom()`) and the **Physical/Virtual Host** (`getDevice()`). For tool executions, the actor is the specific JVM instance (`pid@hostname`) and the device is the hostname. This ensures full traceability in distributed or multi-agent scenarios.
> - **No Reinventing Commons**: Never write code that is already provided by our core dependencies, especially **Apache Commons Lang 3**. For example, use `ExceptionUtils.getStackTrace(e)` instead of writing a custom stack trace stringifier.
> - **No Catching in @AiTools**: Do not use try-catch blocks inside methods annotated with `@AiTool` unless you are performing specific recovery logic or adding high-value context to the error. The tool execution framework automatically catches all `Throwable`s and adds the full stack trace to the tool response.

1.  **JDK 25 Standard**: All modules are built and documented using **JDK 25**. While the target compatibility remains Java 17 for the core engine, the build environment is standardized on the latest LTS/Current release to leverage modern Javadoc features and performance.
2.  **Engineering over Patching**: This is a beta project under active development. **There is no requirement for backwards compatibility.** If a design is flawed or leads to race conditions (e.g., initialization order issues in class hierarchies), **redesign and refactor** the code. Do not add "hacky" null checks or workarounds to patch symptoms of poor engineering.
3.  **No Workarounds for Internal Code**: We are in a pre-live state. If our own code (e.g., `CodeModel`) is failing to resolve types, we fix the underlying logic in the toolkit, we do not create workarounds in the UI or other modules.
4.  **Mandatory Braces**: Always use curly braces `{}` for all control flow statements (`if`, `else`, `for`, `while`, `do`), even when the body contains only a single statement. This improves code clarity and prevents common logic errors during refactoring.
5.  **Javadoc Integrity**: As an open-source Java library, comprehensive documentation is paramount. Existing Javadoc, comments, and blank lines **must never be removed**. **Javadoc is mandatory for everything that can be javadocced (all classes and all methods: public, protected, and private).** Either use the `{@inheritDoc}` feature or provide a concise statement of what the specific implementation is doing.
6.  **Global Javadoc Rules**:
    -   **Mandatory for All Members**: Every class, interface, enum, field (including private), and method (including private and overrides) must have a Javadoc block.
    -   **Meaningful Content**: Avoid "lazy" Javadoc. Instead of just "Returns the name", explain *what* name it returns and its significance in the domain model (e.g., "Returns the unique, dot-separated identifier for the toolkit used in the context hierarchy").
    -   **Implementation Details**: For complex methods, use the Javadoc to explain the internal logic, side effects, and any thread-safety considerations.
7.  **Logging Standard:** All logging **must** be done through the SLF4J API (`@Slf4j`). **Never** use `System.out.println()`. Use placeholders (`{}`) for dynamic content. **All caught exceptions must be logged with the full stack trace. NEVER quietly swallow exceptions or log only the message.**
8.  **Lombok Purity:** Do not add explicit getter or setter methods for fields that are already covered by Lombok annotations (`@Getter`, `@Setter`, `@Data`, etc.). We rely on the build system (Maven) to handle annotation processing correctly. Workarounds for IDE-specific issues are not permitted in the source code.
9.  **DRY (Don't Repeat Yourself):** We strictly avoid duplication of text, logic, or methods. Common principles and standards are defined here in the parent `anahata.md`. Module-specific `anahata.md` files should only contain information unique to that module.
10. **Domain Driven Architecture (DDA):** We strictly adhere to DDA principles. Business logic and state transitions **must reside within the domain model** (`anahata-asi-core`). UI components (`anahata-asi-swing`) are responsible for rendering and user interaction only. **Never** implement business logic (e.g., tool state management, conversation flow) within UI classes.
    - **Domain Model Getters & Logic**: Prefer adding specialized methods (getters or actions) to the domain model over implementing complex logic (e.g. stream/filter/collect) in orchestrators or UI components. This ensures semantically clear domain concepts and promotes business logic reuse.
11. **Reactive UI Updates**: Use `PropertyChangeSource` and `EdtPropertyChangeListener` for all UI-to-Domain bindings. This ensures that UI updates are always performed on the Event Dispatch Thread (EDT) while keeping the domain model thread-safe and decoupled from Swing. Classes that need to fire events should extend `BasicPropertyChangeSource`.
12. **Minimalistic Diff-Based UI Rendering**: To support massive conversations (1000+ messages) and multi-million token context windows, UI components must use a strict diff-based rendering approach. 
    - **Component Reuse**: Never clear and rebuild a container on every update. Use caches to map domain objects to UI components.
    - **Incremental Updates**: Only add, remove, or reorder components that have actually changed in the domain model.
    - **Disciplined Layout Invalidation**: Only call `revalidate()` and `repaint()` when structural changes occur or when a child's preferred size changes. Avoid redundant render cycles at all costs.
13. **"Lucho Style" (Luis Enrique) Development**: We play high-intensity, attacking football. 
    - **No Catenaccio**: Avoid defensive programming patterns like redundant null checks or "quiet catches" for internal architectural components. If a component is required, assume it is present. If it's missing, let it fail fast so we can fix the root cause.
    - **Trust the Architecture**: Rely on the established bidirectional relationships and initialization order. 
    - **Go for the Goal**: Focus on clean, direct implementations. We can "park the bus" with defensive checks once the project goes live.
14. **Cross-Platform Support**: All toolkits and system-level utilities MUST support Linux, Windows, and macOS. Use `OsUtils` and `SystemUtils` to handle platform-specific paths, commands, and behaviors (e.g., lock file names, shell command syntax).
15. **Standard Toolkit Method Order**: To maintain a consistent and predictable structure, all toolkit implementations MUST order their methods as follows:
    1.  `rebind()` (Override)
    2.  `getSystemInstructions()` (Override)
    3.  `populateMessage()` (Override)
    4.  `@AiTool` methods (The public API for the model)
    5.  Public methods that are not `@AiTool`s (if any)
    6.  Private methods (Internal implementation details)

## 7. V2 Context Management (Deep Pinning)

The V2 architecture implements a "Deep Pinning" logic in `AbstractPart.isEffectivelyPruned()`. 
-   **Inherited Pinning:** If a parent `AbstractMessage` is pinned (`pruned = false`), all of its constituent `AbstractPart`s are automatically considered NOT pruned, regardless of their individual `turnsToKeep` or explicit `pruned` state.
-   **Bidirectional Integrity:** The relationship between messages and parts is established in `AbstractMessage.addPart` **before** any property change events are fired, ensuring UI listeners always see a fully initialized state.

## 8. UI & Icon Quality Standards

-   **High-Quality Scaling:** All image scaling (thumbnails and icons) must use `RenderingHints.VALUE_INTERPOLATION_BICUBIC` and `RenderingHints.VALUE_ANTIALIAS_ON`.
-   **`IconUtils`:** Use the `BufferedImage`-based scaling in `IconUtils.getIcon` to ensure sharp, professional-grade icons across the entire Swing UI.

## 9. Dependency Isolation (NBM vs. Standalone)

-   **`commons-io`**: While `tika-core` pulls this in automatically in standalone mode, it must be explicitly bundled in the NetBeans Module (NBM) to avoid `NoClassDefFoundError`. 
-   **Classloader Behavior**: If not declared explicitly in the NBM's POM, the NetBeans isolating classloader appears to "think" the module is attempting to access the `commons-io` version bundled with the `maven-embedder` module, which is not exported. 
-   **NetBeans 280 Context**: We haven't verified if NetBeans RELEASE280 provides its own `commons-io` library module. This behavior is a change from V1, where this explicit declaration wasn't required, and the exact root cause remains under investigation.
-   **Maven Embedder**: The IDE's Maven Embedder uses its own internal `commons-io`. Our bundled version is isolated and does not interfere with IDE operations.

## 10. Session Restoration & `Rebindable`

The V2 framework uses Kryo for high-performance session passivation. Because Kryo bypasses constructors during deserialization, objects that manage transient state (e.g., listeners, locks, or external resource handles) must implement the `Rebindable` interface.
-   **`rebind()` Hook**: This method is automatically called by the `RebindableWrapperSerializer` after an object has been fully read from the stream.
-   **Circular Dependencies & Null Guards**: While `rebind()` is called after the object itself is read, circular dependencies in the object graph mean that some **non-transient** fields (references to other objects) might still be null or partially initialized when `rebind()` is executing. Always include null guards in `rebind()` and in methods that access these fields to ensure robustness during the restoration process.

## 11. V2 Tool Framework: DTO & Map Support

The V2 tool framework supports complex Java types as tool parameters.
-   **DTO Support**: `List<DTO>` and nested POJOs are fully supported. The `SchemaProvider` automatically generates OpenAPI 3-compliant schemas for these types.
-   **Map Limitations**: Due to current limitations in the underlying GenAI schema generation, `Map<String, Map<String, String>>` (nested maps) may not be correctly represented if `additionalProperties` features are required. Simple `Map<String, String>` or `List<DTO>` are preferred for complex data structures.
-   **Custom Schemas**: The framework allows bypassing native GenAI schema objects if necessary, but standard POJO-based DTOs are the recommended approach for type safety and clarity.

## 12. JIT Testing & "Compile on Save" Delays

When using `NetBeansProjectJVM.compileAndExecuteInProject` to test code that has just been modified via `suggestChange` or `writeFile`, you must account for the asynchronous nature of the IDE's "Compile on Save" feature.
-   **Mandatory Delay**: Always include a 1-2 second delay (e.g., `Thread.sleep(2000)`) at the beginning of your `Anahata.call()` method if you are testing logic that depends on the latest version of a file in the project.
-   **Alternative**: Check `SourceUtils.isScanningInProgress()` or similar IDE state indicators to ensure the project is ready before execution.

## 13. Working Directory Structure

The Anahata framework uses a dedicated directory in the user's home folder for persistent state.

-   **V1 (Legacy)**: `~/.anahata/ai-assistant`
-   **V2 (Current)**: `~/.anahata/asi` (Renamed from `ai` to align with ASI branding)
