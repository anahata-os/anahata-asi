# Anahata AI V2 Tool Framework

This document provides a comprehensive overview of the Anahata AI V2 tool framework, a sophisticated, annotation-driven system for defining, managing, and executing Java-based tools for Large Language Models (LLMs).

## 1. Core Philosophy

The V2 framework is built on a foundation of **decoupling, type safety, and automation**. It aims to make the process of creating and exposing Java methods as tools as seamless as possible, while providing a rich, model-agnostic domain that ensures robustness and extensibility.

## 2. The Lifecycle of a Tool

The entire process, from a simple Java method to a model-callable function, is handled by the framework in a series of automated steps.

### 2.1. Definition: Annotation-Driven Discovery

Tools are defined in standard Java classes using a set of intuitive annotations:

-   **`@AiToolkit`**: Marks a class as a container for related tools. It provides a high-level description for the entire toolkit.
-   **`@AiTool`**: Marks a public method within a toolkit class as an AI-callable tool. It contains the detailed description that the model will use to understand the tool's purpose.
-   **`@AIToolParam`**: Describes a parameter of a tool method, providing the model with essential information about what to pass as an argument.

### 2.2. Registration and Parsing

At application startup, the `ToolManager` is responsible for discovering and registering all annotated tool classes.

1.  **Discovery**: The `ToolManager.registerClasses(...)` method is called with the toolkit classes.
2.  **Parsing**: For each class, a `JavaObjectToolkit` is created. This object uses reflection to parse the class, its methods, and its parameters, building a rich, in-memory representation of the toolkit.
3.  **Domain Model**: This process creates a tree of rich domain objects:
    -   `JavaObjectToolkit` holds the toolkit's metadata and a list of...
    -   `JavaMethodTool` objects, each representing a single tool and containing its `java.lang.reflect.Method`, which in turn holds a list of...
    -   `JavaMethodToolParameter` objects, each containing its `java.lang.reflect.Parameter` and full generic `Type`.

### 2.3. Schema Generation: The Role of `SchemaProvider`

This is the cornerstone of the framework's intelligence. Once the tool is parsed into the domain model, the `SchemaProvider` is invoked to automatically generate a detailed, **OpenAPI 3.0 compliant JSON schema**.

-   **Deep Reflection**: The `SchemaProvider` performs a deep, recursive analysis of the tool's return type and parameter types.
-   **Rich Type Information**: It enriches the schema by embedding the fully qualified Java class name into the `title` field of every object, property, and array item. This provides the model with unambiguous type information.
-   **Automatic Response Wrapping**: The provider intelligently wraps the tool's actual return type within a standardized response schema (`JavaMethodToolResponse`). This wrapper includes common fields like `status`, `logs`, and `attachments`.
-   **Void Method Handling**: If a tool method returns `void` or `Void`, the `SchemaProvider` automatically removes the `result` property from the final schema, correctly signaling to the model that no return value should be expected.
-   **Robust Inlining**: The provider correctly handles complex, nested, and recursive object graphs. It generates a complete schema map with internal references (`$ref`) first, then performs a single, final inlining pass to produce a clean, fully resolved schema. This robust order of operations prevents bugs with nested types.

### 2.4. Execution: A Type-Safe, Deferred Process

When the model requests a tool call, the framework follows a robust, multi-step process:

1.  **Call Creation**: The `ToolManager` creates a `JavaMethodToolCall` object, which represents the model's request.
2.  **Argument Conversion**: The `JavaMethodTool` uses the rich `JavaMethodToolParameter` models to perform a **type-safe conversion** of the model's JSON arguments into the precise Java types required by the method signature. It uses `Gson` and the stored generic `Type` for each parameter to handle complex types like `List<String>` correctly.
3.  **Deferred Execution**: The call is not executed immediately. Instead, a `JavaMethodToolResponse` object is created and paired with the call.
4.  **Invocation**: The `JavaMethodToolResponse.execute()` method is called. This method uses the stored `java.lang.reflect.Method` and the converted arguments to invoke the actual Java tool method.
5.  **Result Capturing**: The result, or any exception, is captured in the `JavaMethodToolResponse` object, which is then sent back to the model.

This architecture ensures a clean separation of concerns, robust error handling, and a high degree of type safety throughout the entire tool lifecycle.

### 2.5. V3 Context-Aware Tools

The V3 architecture introduces a new layer of intelligence, allowing tools to participate in their own context lifecycle.

-   **Tool-Driven Retention:** A tool can now dynamically set its own retention policy based on its execution outcome. For example, a `suggestChange` tool can set its `turnsToKeep` to `0` on success (as the change is now in the source code) but set it to `10` on failure or rejection, ensuring the model retains the context of the failed interaction.
-   **Model-Driven Retention:** A new tool, `ContextWindow.setTurnsToKeep(long partId, int turns)`, will be introduced. This will give the model direct, granular control over the context, allowing it to "pin" critical information (like a `git diff` or a specific error message) for as long as it's needed.
