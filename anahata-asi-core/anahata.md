/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata AI Core Framework (`anahata-asi-core`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This document outlines the vision and architecture of the `anahata-asi-core` project, which serves as the core, model-agnostic AI framework.

> [!CAUTION]
> **PARAMOUNT PRINCIPLES: SIMPLICITY AND STABILITY**
> The absolute priority for all development is **Simplicity and Stability** (or Stability through Simplicity). These principles rule above all others. 
> - **Core Discussion**: Any proposed changes to this module **MUST** be discussed and agreed upon with the user in the conversation before calling `updateTextFile`.
> - **No Dirty Hacks**: Avoid "dirty hacks" or workarounds (e.g., `SwingUtils.runInEDT` should be used for UI-bound logic, but domain logic must remain thread-safe and decoupled). If a design leads to race conditions, it requires proper refactoring.
> - **Unified Content API**: Always prefer `message.addTextPart(text)` or `message.addBlobPart(...)` over direct instantiation of `TextPart` or `BlobPart`.
> - **API Leanliness**: Avoid redundant signatures or secondary constructors. Keep the API lean and consistent.

## 1. Vision & Goal

The primary goal is to create a robust, extensible, and model-agnostic AI framework in Java. This core library defines a standard set of interfaces and a rich domain model for interacting with Large Language Models (LLMs).

This project contains the foundational logic, while provider-specific implementations are developed in separate "adapter" projects (e.g., `anahata-asi-gemini`).

---

## 2. V2 Core Architecture Summary

### 2.1. Core Orchestration (`uno.anahata.asi.agi`)

-   **`Agi`**: The central orchestrator for a conversation session.
-   **`AgiConfig`**: A blueprint object that defines the configuration for an `Agi` session.

### 2.2. Model-Agnostic Domain (`uno.anahata.asi.model.*`)

-   **Conversation Primitives (`.core`)**: `AbstractMessage`, `AbstractPart`, `UserMessage`, `AbstractModelMessage`.
-   **Provider Abstraction (`.provider`)**: `AbstractAgiProvider`, `AbstractModel`.
-   **Tooling Model (`.tool`)**: `AbstractToolkit`, `AbstractTool`, `AbstractToolCall`, `AbstractToolResponse`.

### 2.3. V2 Context Management

-   **Self-Contained Logic**: `AbstractPart` contains the primary logic for pruning (`isEffectivelyPruned()`).
-   **Two-Phase Pruning**: Soft prune (filtered from payload) and Hard prune (permanent deletion).
-   **`ContextManager`**: Orchestrates prompt assembly and system instruction injection.

### 2.4. V2 Tool Framework (`uno.anahata.asi.tool.*`)

-   **Annotation-Driven**: Tools defined using `@AiToolkit`, `@AiTool`, and `@AiToolParam`.
-   **`SchemaProvider`**: Automatically generates OpenAPI 3 schemas from Java types.
-   **`ToolContext`**: Provides a context-aware API (via `ThreadLocal`) for logging and session access.

### 2.5. Resource & State Management (`uno.anahata.asi.resource.*`)

-   **`ResourceManager`**: Manages all stateful resources.
-   **`AbstractResource`**: Base class unifying concepts of resource and context provider.
-   **`AbstractPathResource`**: specialized base for file-based resources.
