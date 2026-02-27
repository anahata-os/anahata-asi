/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Gemini Provider (`anahata-asi-gemini`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides the adapter implementation for the Google Gemini API.

## 1. Purpose

This module's sole responsibility is to act as an **Adapter** between the Google Gemini API and the core `anahata-asi` framework.

## 2. Key Components

-   **`GeminiAgiProvider`:** The concrete implementation of `AbstractAgiProvider`.

-   **`GeminiModel`:** A wrapper around the native Google GenAI `GenerativeModel`. Implements `generateContent`.

-   **`adapter` Package:** Logic for translating between core domain models and native Gemini API types.

## 3. CLI Test Harness

-   **`GeminiCliAgiConfig.java`:** Registers the `GeminiAgiProvider` for CLI usage.

-   **`CliMain.java` (in Standalone)**: Launcher for the core CLI with this provider.
