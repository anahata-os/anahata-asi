# Anahata AI Gemini Provider

This document provides an overview of the `anahata-ai-gemini` module, which serves as the reference implementation for the V2 provider architecture.

## 1. Purpose

This module's sole responsibility is to act as an **Adapter** between the Google Gemini API and the core `anahata-ai` framework. It demonstrates how to build a provider that is fully compatible with the V2 architecture, including the `Chat`-centric lifecycle, model-led content generation, and the decoupled CLI test harness.

## 2. Key Components

-   **`GeminiAiProvider`:** The concrete implementation of the `AbstractAiProvider`. It uses a no-argument constructor and is initialized via the `init(Chat)` method. It also includes a "fail-fast" check for the Gemini API key.

-   **`GeminiModel`:** A wrapper around the native Google GenAI `Model` object. It implements the `generateContent(Request)` method, which is the core of the V2 design.

-   **`adapter` Package:** This package contains all the logic for translating between the core `anahata-ai` domain model (e.g., `Request`, `Response`, `ModelMessage`) and the Google GenAI API's native types (e.g., `GenerateContentResponse`, `Content`).

## 3. CLI Test Harness

This module provides a complete, self-contained example of how to test a provider using the core command-line interface.

-   **`GeminiCliChatConfig.java`:** A simple class that extends `ChatConfig` and registers the `GeminiAiProvider`. The core CLI discovers this class at runtime via reflection.

-   **`Main.java`:** A minimal launcher class whose only job is to call the `main` method of the core `uno.anahata.ai.Cli`. This demonstrates the decoupled nature of the test harness.
