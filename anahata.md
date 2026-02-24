/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Maven Multi-Module Project

This document provides the high-level strategic overview and rules for all modules under the `anahata-ai-parent` project.

## 2. Core Modules

The project is divided into the following active modules:

1. **`anahata-asi-core`**: The foundational, model-agnostic framework. Contains all core interfaces, the domain model, and the tool-chain.
2. **`anahata-asi-gemini`**: The first provider-specific implementation, acting as an Adapter between the Google Gemini API and the core framework.
3. **`anahata-asi-swing`**: A reusable, provider-agnostic Swing UI component for building agentic workflows.
4. **`anahata-asi-cli`**: The command-line interface for interacting with JASI.
5. **`anahata-asi-standalone`**: A standalone Java application for running JASI outside of an IDE.
6. **`anahata-asi-web`**: The official JASI Portal and documentation hub.
7. **`anahata-asi-nb`**: The V2 NetBeans integration module.
8. **`anahata-asi-yam`**: The "Yet Another Module" for creative and experimental agentic tools.

## 3. Strategic Documents

This project uses a set of key documents to guide development. For detailed information, please refer to the following:

- **`ci.md`**: Contains the CI/CD strategy, website deployment details, and Javadoc configuration notes.

## 4. Coding Principles

> [!NOTE]
> **Simplicity and Stability**
> The absolute priority for all development is **Simplicity and Stability** (or Stability through Simplicity). These principles rule above all others. 

- **Ground Truth Discovery**: Never attempt to write or refactor code without first loading the relevant Java types (classes, interfaces, etc.) into your context. Use `LocalFiles.readFile` or `JavaSources.getSource`.
- **Architectural Integrity**: We do not implement "dirty hacks" or workarounds to mask architectural flaws. If a design is broken, we fix the design.
- **JDK 25 Standard**: All modules are built and documented using **JDK 25**.
- **Engineering over Patching**: There is no requirement for backwards compatibility in this beta stage. Redesign flawed components instead of adding null checks.
- **Unified Content API**: Always prefer `message.addTextPart(text)` or `message.addBlobPart(...)` over direct instantiation of `TextPart` or `BlobPart`.
- **API Leanliness**: Avoid redundant signatures or secondary constructors. Keep the API lean and consistent.
- **Identity & Distributed Observability**: Message metadata must distinguish between the Logical Actor (`getFrom()`) and the Physical/Virtual Host (`getDevice()`).
- **No Reinventing Commons**: Use existing libraries like **Apache Commons Lang 3**.
- **Clean Execution**: Do not use try-catch blocks inside `@AiTool` methods unless performing specific recovery. The framework handles exceptions automatically.
- **Fail Fast**: Avoid defensive programming like redundant null checks for internal components. Let it fail so root causes can be fixed.
- **Mandatory Braces**: Always use curly braces `{}` for all control flow statements (`if`, `else`, `for`, `while`, `do`).
- **Logging Standard**: Use SLF4J (`@Slf4j`) for all logging. Never use `System.out.println()`.
- **Lombok Purity**: Rely on Maven for Lombok annotation processing; do not add explicit getters/setters for Lombok-managed fields.
- **Domain Driven Architecture (DDA)**: Business logic and state transitions must reside in `anahata-asi-core`. UI components in `anahata-asi-swing` are for rendering only.
- **Reactive UI**: Use `PropertyChangeSource` and `EdtPropertyChangeListener` for UI-to-Domain bindings to ensure EDT execution.
- **Cross-Platform Support**: All toolkits and utilities must support Linux, Windows, and macOS via `OsUtils` and `SystemUtils`.
- **Standard Toolkit Method Order**: 
    1. `rebind()`
    2. `getSystemInstructions()`
    3. `populateMessage()`
    4. `@AiTool` methods
    5. Public helper methods
    6. Private implementation details

## 5. Javadoc Standards

Comprehensive documentation is mandatory for this open-source project. Existing Javadoc and comments must never be removed.

- **Mandatory Documentation**: Javadoc is required for **everything** that can be javadocced: all classes, interfaces, enums, fields (including private), and methods (including public, protected, private, and overrides).
- **Inheritance**: Use `{@inheritDoc}` or provide a concise explanation of the specific implementation.
- **Meaningful Content**: Avoid "lazy" Javadoc. Explain the significance and semantics within the domain model.
- **Implementation Details**: For complex logic, use Javadoc to explain internal side effects and thread-safety considerations.


### 6.6. JIT Testing
- **Compilation Delay**: When testing modified code via `NetBeansProjectJVM`, include a 1-2 second delay at the start of `Anahata.call()` to allow "Compile on Save" to finish.

## 7. Environment

- **Working Directory**: `~/.anahata/asi` (Standardized for V2).

Força Barça!
