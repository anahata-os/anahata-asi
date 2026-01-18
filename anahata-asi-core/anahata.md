/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata AI Core Framework

This document outlines the vision and architecture of the `anahata-ai-core` project, which serves as the core, model-agnostic AI framework.

## 1. Vision & Goal

The primary goal is to create a robust, extensible, and model-agnostic AI framework in Java. This core library defines a standard set of interfaces and a rich domain model for interacting with Large Language Models (LLMs), allowing developers to build AI-powered applications without being locked into a specific provider (e.g., Google Gemini, OpenAI, Anthropic).

This project contains the foundational logic, while provider-specific implementations are developed in separate "adapter" projects (e.g., `anahata-ai-gemini`).

---

## 2. V2 Core Architecture Summary

This section provides a high-level summary of the key architectural components and design patterns within the `anahata-ai-core` module.

### 2.1. Core Orchestration (`uno.anahata.ai.chat`)

-   **`Chat`**: The central orchestrator for a conversation session. It manages the main loop, message sending, and interaction with the selected AI model.
-   **`ChatConfig`**: A blueprint object that defines the configuration for a `Chat` session, including available AI providers, tool classes, and context management policies (e.g., pruning delays, retention turns).

### 2.2. Model-Agnostic Domain (`uno.anahata.ai.model.*`)

This is the foundation of the framework, ensuring decoupling from any specific AI provider.

-   **Conversation Primitives (`.core`)**:
    -   `AbstractMessage`: Base class for all messages (`UserMessage`, `AbstractModelMessage`, `AbstractToolMessage`). Crucially, it holds a reference to the parent `Chat`, making the entire domain model context-aware.
    -   `AbstractPart`: Base for message content (`TextPart`, `BlobPart`, `AbstractToolCall`). Contains the core logic for the V2 context management system.
-   **Provider Abstraction (`.provider`)**:
    -   `AbstractAiProvider`: Defines the contract for an AI provider (e.g., Gemini, OpenAI), responsible for listing models and managing API keys.
    -   `AbstractModel`: Represents a specific model (e.g., `gemini-1.5-pro`), responsible for the core `generateContent` action.
-   **Tooling Model (`.tool`)**:
    -   A rich, model-agnostic representation of tools (`AbstractToolkit`, `AbstractTool`), their parameters (`AbstractToolParameter`), and their lifecycle (`AbstractToolCall`, `AbstractToolResponse`).

### 2.3. V2 Context Management (`ContextManager` & Core Domain)

A sophisticated system built directly into the domain model.

-   **Self-Contained Logic**: The `AbstractPart` class contains the primary logic for pruning. Methods like `isEffectivelyPruned()` and `getTurnsLeft()` make each part aware of its own state within the conversation's timeline.
-   **Two-Phase Pruning**:
    1.  **Soft Prune**: "Effectively pruned" parts are filtered out of the payload sent to the model but remain in the session history.
    2.  **Hard Prune**: A background process permanently deletes parts that have been soft-pruned for a configurable number of turns (`hardPruneDelay` in `ChatConfig`).
-   **`ContextManager`**: Orchestrates the assembly of the prompt, including injecting system instructions from various providers (`AbstractSystemInstructionsProvider`).

### 2.4. V2 Tool Framework (`uno.anahata.ai.tool.*`)

A powerful, reflection-based system for defining and executing local Java tools. For a more detailed breakdown, please see `tools.md`.

-   **Annotation-Driven**: Developers define tools using `@AiToolkit`, `@AiTool`, and `@AIToolParam`.
-   **Rich Metadata**: `ToolManager` parses these annotations into a rich object model (`JavaObjectToolkit`, `JavaMethodTool`), which includes Java `Method` and `Type` information for type-safe argument conversion.
-   **`SchemaProvider`**: Automatically generates detailed OpenAPI 3-compliant JSON schemas from Java types, ensuring the model has a precise understanding of tool signatures.
-   **`AbstractJavaTool`**: An optional base class for toolkits that provides a context-aware API (via `ThreadLocal`) for tool methods to log messages or access the `Chat` session without requiring it as a method parameter.

### 2.5. Resource & State Management (`uno.anahata.ai.resource.*`)

A robust framework for managing stateful entities within the chat context. For a more detailed breakdown, please see `resources.md`.

-   **`ResourceManager`**: Manages all stateful resources within the chat.
-   **`AbstractResource`**: The base class for stateful entities. It unifies the concept of a resource with a context provider, allowing each resource to define its own refresh policy (`RefreshPolicy`) and position in the prompt (`ContextPosition`).
-   **`AbstractPathResource`**: A specialized base for file-based resources, implementing an "Atomic Reload" architecture to handle stale files. `TextFileResource` and its `TextViewport` provide a concrete implementation for viewing and paginating text files.
