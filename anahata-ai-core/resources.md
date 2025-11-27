# V2 Resource Management Framework Summary

This document provides a comprehensive summary of the V2 resource management framework, a robust, stateful, and extensible system for managing entities within the AI's context.

## 1. Core Architectural Principles

-   **Unified Resource Model (`AbstractResource`):** The concepts of a "stateful resource" and a "context provider" are merged into a single, powerful `AbstractResource` base class. Every managed resource is a self-contained entity that knows its own identity (`id`, `name`), its lifecycle policy (`RefreshPolicy`), and where it belongs in the prompt (`ContextPosition`).
-   **Type-Safe Inheritance Hierarchy:** A clean inheritance model (`AbstractResource` -> `AbstractPathResource` -> `TextFileResource`) ensures type safety and extensibility for future resource types.
-   **Lazy Loading & Lightweight Handles:** Resource objects are designed as lightweight handles. They store a `java.nio.file.Path` to the resource, not the full content in memory, making them memory-efficient and easily serializable.
-   **On-Demand, Self-Rendering Views:** The `populate(RagMessage)` method on a resource is the "smart accessor." It renders the resource's content just-in-time into a `RagMessage` provided by the `ContextManager`. For text files, this is managed by a powerful, character-based `TextViewport` that handles pagination, line wrapping, and filtering, giving fine-grained control over the token count of the output.
-   **Atomic, Auto-Refreshing Logic:** The `populate()` method contains the "auto-healing" logic. It automatically checks if a file on disk is stale (`isStale()`) and, if the `RefreshPolicy` is `LIVE`, triggers an atomic `reload()` operation before rendering the view. This ensures the context is always up-to-date without manual intervention.
-   **Efficient Tool Responses:** Through the strategic use of `@JsonIgnore`, when a tool like `Files.loadTextFile()` returns a resource object, the JSON response is a lightweight handle containing only metadata, not the redundant and token-heavy file content.

## 2. Framework Integration

-   **`ResourceManager` (The Container):** This class acts as the central, ordered container (`LinkedHashMap`) for all registered `AbstractResource` instances.
-   **`ContextManager` (The Injector):** The `ContextManager` queries the `ResourceManager` on every turn and intelligently injects the rendered parts of each resource into the correct location in the prompt (`SYSTEM_INSTRUCTIONS` or `PROMPT_AUGMENTATION`).
-   **Dedicated Toolkits (`Files.java`):** The framework uses dedicated, professional-grade toolkits for each resource domain. The `Files.java` toolkit demonstrates this with robust error handling (`AiToolException`), clear self-documentation (`@AiTool`, `@AIToolParam`), and efficient context management (`retention = 0`).

The result is a complete, end-to-end resource management system that is a foundational pillar of the V2 framework.
