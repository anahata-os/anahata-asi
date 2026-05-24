/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Maven Multi-Module Project

This document provides the high-level strategic overview and **rules that apply all modules** under the `anahata-ai-parent` project.

## 2. Core Modules

The project is divided into the following active modules:

1. **`anahata-asi-core`**: The foundational, model-agnostic framework. Contains all core interfaces, the domain model, and the tool-chain.
2. **`anahata-asi-gemini`**: The first provider-specific implementation, acting as an Adapter between the Google Gemini API and the core framework.
3. **`anahata-asi-swing`**: A reusable, provider-agnostic Swing UI component for building agentic workflows.
4. **`anahata-asi-cli`**: A draft command-line interface for interacting with ASI, semi discontinued. 
5. **`anahata-asi-standalone`**: A standalone Java application for running ASI outside of an IDE.
6. **`anahata-asi-web`**: The official ASI Portal and documentation hub.
7. **`anahata-asi-nb`**: The NetBeans integration module.
8. **`anahata-asi-yam`**: The "Yet Another Module" for creative and experimental agentic tools.

## 3. Strategic Documents

This project uses a set of key documents to guide development. For detailed information, please refer to the following:

- **`ci.md`**: Contains the CI/CD strategy, website deployment details, and Javadoc configuration notes.

## 4. Coding Principles

> [!NOTE]
> **Simplicity and Stability**
> The absolute priority for all development is **Simplicity and Stability** (or Stability through Simplicity). These principles rule above all others. 

- **Domain Driven Architecture (DDA)**: The entire multimodule project is based on DDA. Business logic and state transitions must reside in the domain model entities. Anything UI agnositc should be in `anahata-asi-core`. UI components of core in `anahata-asi-swing`.
- **Architectural Integrity**: We do not implement "dirty hacks" or workarounds to mask architectural flaws. If a design is broken, we fix the design.
- **JDK 25 Standard**: All modules are built and documented using **JDK 25**.
- **Engineering over Patching**: There is no requirement for backwards compatibility in this beta stage. Redesign flawed components instead of adding null checks.
- **Unified Content API**: Always prefer `message.addTextPart(text)` or `message.addBlobPart(...)` over direct instantiation of `TextPart` or `BlobPart`.
- **API Leanliness**: Avoid redundant signatures or secondary constructors. Keep the API lean and consistent.
- **Identity & Distributed Observability**: Message metadata must distinguish between the Logical Actor (`getFrom()`) and the Physical/Virtual Host (`getDevice()`).
- **No Reinventing Commons**: Use existing libraries like **Apache Commons Lang 3**.
- **Fail Fast**: Avoid defensive programming like redundant null checks for internal components. Let it fail so root causes can be fixed. 
- **No Method-Start Null Checks**: You are strictly forbidden from starting a method with a null check on parameters for the sake of defensive programming (e.g., `if (other == null) return;`). Let the JVM throw the NullPointerException so the caller can be corrected.
- **No Quietly Catching Exceptions**: You are strictly forbidden from catching exceptions and doing nothing. All exceptions should be logged. 
- **Clean Execution**: Do not use try-catch blocks inside `@AgiTool` methods unless performing specific recovery. The framework handles exceptions automatically. If you need to throw an error intended for the user, prefer throwing an `AgiToolException` to ensure a clean message without stack traces.
- **Mandatory Braces**: Always use curly braces `{}` for all control flow statements (`if`, `else`, `for`, `while`, `do`). Single-line lambdas without braces (e.g., `list.stream().filter(m -> m.isCool())...`) are perfectly fine and often preferred for readability.
- **Logging Standard**: Use SLF4J (`@Slf4j`) for all logging. Never use `System.out.println()`.
- **Lombok Purity**: Rely on Lombok annotation processing; do not add explicit getters/setters for Lombok-managed fields.
- **Serialization (JsonIgnore/Schema vs Kryo)**: The `com.fasterxml.jackson.annotation.JsonIgnore` and `io.swagger.v3.oas.annotations.media.Schema` annotations are only used for generating json schemas and serializing/deserializing DTOs during transport between the **LLM Model API** and the **Anahata framework**. They are not used for persisting internal framework/session state (we use Kryo for that) Kryo serializes the entire Agi object graph (includes the agi itself, including all messages in the history with all their tool calls and responses, the resources, toolkit instances, context providers, and anything else in the Agi object graph during every turn and every time a tool gets executed. Use these @JsonIgnore or @Schema(hidden=true) only in DTOs that are used as tool call parameters or returned types to keep the tool definitions and tool responses lean. Fields that cannot or should not be serialized by Kryo on every autobackup pulse should be marked as 'transient' (with the java transient modifier) and can be recovered early with the rebind() method of Rebindable or the postActivate() method in AnahataToolkit instances (postActivate gets invoked after the session has been fully restored while rebin() is invoked ealier during deserialization as soon as that object is deserialized. 
- **Static should be static**: A method that does not use instance members should be made static.
- **Thread Awareness**: Toolkit methods can be invoked from background threads (during AI tool execution) or the Event Dispatch Thread (when triggered by user UI actions). The `SwingUtils` class contains convenience methods like `runInEDT` and `runInEDTAndWait` to help handle these transitions safely.
- **Reactive UI**: Use `PropertyChangeSource` and `EdtPropertyChangeListener` for UI-to-Domain bindings to ensure EDT execution.
- **Cross-Platform Support**: All toolkits and utilities must support Linux, Windows, and macOS via `OsUtils` and `SystemUtils`.
- **Standard Toolkit Method creation**: 
    1. extend `AnahataToolkit` and annotate the class with @AgiToolkit("what this toolkit does")
    2. do not implement ContextProvider methods like getId() or getParentContextProvider() or setProviding() as this is already handled by the base AnahataToolkit class.
    3. @AgiTool annotated methods should not quietly catch exceptions nor simply log caugh exceptions to the slf4j logger, they should log errors to the tools error stream using error() so the model can see the error.
    4. @AgiTool methods can throw an AgiToolException or any other Exception. AgiToolExceptions only dump the exception message to the tools error field, other exceptions will dimp the full stack trace into the tools errors
    5. @AgiTool methods should use log("") for logs the model should see in the tools response logs (the model doesn't see slf4j logging like log.info or log.warn or log.error).

- **Standard Toolkit Method Order**: 
    1. `rebind()` (if needed during early stages of deserilization)
    2. `postActivate()` (call back hook when the Agi session has been completely deserialized and bound to the AsiContainer)
    3. `getSystemInstructions()` (if needed, with instructions that are common to all @AgiTools)
    3. `populateMessage(RagMessage)` (for augmented context on every turn)
    4. `@AgiTool` methods with @AgiToolParam annotations on parameters (If using pojos in parameters or returned types, use @Schema, @JsonIngore, etc type of annotations to control schema definitions and serialization mappings)
    5. Public helper methods (if needed that can be accessed by other toolkits via getToolkit(OtherToolkit.class) or by any other classess of the host application. Calling one toolkit from another toolkit supports context propagation via thread local of the associated ToolContex (this works for all methods of ToolContext such as log(""), error(""), addAttachment(), getModelId(), etc.) 
    6. Private implementation details (internal private methods )

- **NO fqns on method bodies**: 
    1. Never add fqns inside a class, looks terrible. If you are using the resources toolkit, you need to include the imports. 

## 5. Javadoc Standards

Comprehensive documentation is mandatory for this open-source project. Existing Javadoc and comments must never be removed.

- **Mandatory Visibility**: Javadoc is mandatory for **ALL** visibilities: `public`, `protected`, `package-private`, and `private`. If it can be Javadocced, it must be Javadocced.
- **Javadocs should always be above any annotations**: if a class or a method has annotations, the javadoc should always be above the annotations.
- **ASI-Grade Quality**: Javadocs must be meaningful, providing architectural context, thread-safety notes, and domain-level significance. Avoid "lazy" or redundant Javadoc that just repeats the method name.
- **Override/Implementation Logic**: Every override or implementation must use the following javadoc pattern:
    `/** 
         {@inheritDoc} 
         <p>Describe the specific implementation logic here, explaining why and how this member is being overridden/implemented.</p> 
      */
     @Override`
- **Implementation Details**: For complex logic, use Javadoc to explain internal side effects and thread-safety considerations.

## 6. Lifecycle Management

- **The rebind() Hook**: The `rebind()` method in the `uno.anahata.asi.persistence.Rebindable` interface is strictly reserved for recovering transient fields and re-establishing listeners after deserialization (e.g., from Kryo). It must never be called programmatically from business logic or constructors. Implementations must always call `super.rebind()` to ensure parent recovery.

## 7. Evolutionary Status

This project is in a pre-production state. We value architectural purity and long-term maintainability.

- **Architectural Rework**: If you identify a cleaner or more efficient design pattern, you are encouraged to propose and implement a rework of existing structures. Refactoring for clarity and future-proofing is preferred over applying patches to flawed designs.

## 8. Serialization Annotations

- **JsonIgnore and Schema**: The `com.fasterxml.jackson.annotation.JsonIgnore` and `io.swagger.v3.oas.annotations.media.Schema` annotations are strictly reserved for controlling the serialization of DTOs during transport between the **LLM Model API** and the **Anahata framework**. They must **never** be interpreted as controlling internal framework state (Kryo) or UI-level object mapping. Use these to keep the AI's prompt lean, but ensure critical state fields remain visible to the framework's internal logic.

## 9. Environment

- **Working Directory**: `~/.anahata/asi` (Standardized for V2).

Força Barça!
