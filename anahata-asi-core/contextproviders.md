# Anahata AI V2 Context Provider Framework

This document describes the hierarchical context provider system, which allows for structured, just-in-time injection of system instructions and RAG data into AI requests.

## 1. Core Philosophy

The V2 context system is built on a **hierarchical tree model**. This allows complex components (like the `ToolManager`) to act as root providers that manage a collection of child providers (individual toolkits).

## 2. The `ContextProvider` Interface

The `ContextProvider` interface defines the contract for any component that contributes to the AI's context.

-   **`isProviding()`**: Determines if the provider is currently active.
-   **`getFullyQualifiedId()`**: Returns a unique, dot-separated ID reflecting the provider's position in the hierarchy (e.g., `core-tool-manager.Files`).
-   **`getSystemInstructions(Chat chat)`**: Returns a list of strings to be prepended to the conversation as system-level guidance.
-   **`populateMessage(RagMessage ragMessage)`**: Injects dynamic, just-in-time data into a synthetic `UserMessage` at the end of the prompt (RAG).

## 3. Hierarchical Management

Providers can be nested using `getParentProvider()` and `getChildrenProviders()`.

-   **`getFlattenedHierarchy(boolean enabledOnly)`**: A powerful default method that traverses the tree and returns a flat list of all active providers.
-   **`ContextManager` Integration**: The `ContextManager` iterates through all registered root providers and uses the flattened hierarchy to assemble the final prompt payload.

## 4. Key Implementations

-   **`BasicContextProvider`**: A simple, reusable implementation of the hierarchy logic.
-   **`ToolManager`**: The root provider for all AI tools. It injects metadata about enabled and disabled toolkits.
-   **`AnahataToolkit`**: The base class for toolkits. It implements `ContextProvider`, allowing any toolkit to provide its own specific instructions or RAG data (e.g., the `Shell` toolkit providing environment variables).
-   **`CoreContextProvider`**: Provides the foundational system instructions and "personality" for the Anahata ASI.

## 5. UI Integration

The Swing UI (`ToolkitDetailPanel`) uses the provider hierarchy to generate real-time previews of the instructions and RAG data that a specific toolkit is contributing to the context.
