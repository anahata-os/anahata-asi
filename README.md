# Anahata AI: Enterprise-Grade AI Framework for Java

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/pablo-anahata/anahata-ai-parent/actions) 
[![License](https://img.shields.io/badge/license-Anahata%20(ASL%20V108)-blueviolet)](LICENSE)

**Anahata AI** is a pure-Java, enterprise-grade framework for building sophisticated, context-aware AI assistants. Its core strength is a powerful, annotation-driven local tool system that enables an AI to execute arbitrary Java code, interact with the local file system, and manage application state. Designed for deep integration into IDEs and standalone desktop applications, it includes a complete, embeddable Swing UI for a rich, interactive chat experience.

## ✨ Core Features

*   **Model-Agnostic Core:** A robust, extensible framework that defines a standard set of interfaces and a rich domain model for interacting with any LLM. Switch between providers like Google Gemini, OpenAI, or Anthropic by simply adding a new adapter.
*   **Powerful V2 Tool Framework:** Define complex tools in plain Java using simple annotations (`@AiToolkit`, `@AiTool`). The framework handles automatic JSON schema generation, type-safe argument conversion, and the entire tool execution lifecycle.
*   **`Chat`-Centric Architecture:** A clean, object-oriented design where the `Chat` class acts as the central orchestrator for the conversation history, provider lifecycle, and tool management.
*   **Decoupled CLI Test Harness:** Provider modules can be tested end-to-end using a core command-line interface that discovers and loads providers at runtime via reflection.
*   **Embeddable Swing UI:** A reusable `anahata-ai-swing` module provides a ready-to-use UI component for building interactive AI applications.

## 📦 Modules

This project follows a multi-module architecture:

-   **`anahata-ai` (Core Framework):** Contains the model-agnostic interfaces, the rich domain model (`Request`, `Response`, `AbstractMessage`), and the complete V2 tool framework. This is the foundational JAR for all other modules.
-   **`anahata-ai-gemini` (Gemini Provider):** The reference implementation of a provider adapter. It translates between the core Anahata AI domain and the Google Gemini API.
-   **`anahata-ai-swing` (UI Layer):** Provides the embeddable Swing UI components for building rich client applications.

## 🚀 Getting Started

The easiest way to test the framework is to run the command-line interface (CLI) from the Gemini provider module.

1.  **Configure API Key:** Create a file at `~/.anahata/ai/gemini/api_keys.txt` and add your Google Gemini API key to it.
2.  **Build the Project:**
    ```bash
    mvn clean install
    ```
3.  **Run the CLI:**
    ```bash
    cd anahata-ai-gemini
    mvn exec:java
    ```

This will launch an interactive chat session in your terminal.

## 🛠️ Building

To build all modules from the parent directory, run:

```bash
mvn clean install
```

## 📜 Licensing

This project is licensed under the **[Anahata Software License (ASL) V108, "The Immutable Edict" Edition](LICENSE)**. This is the final and unchangeable license for this project.

It is a unique document crafted by the Anahata AI Assistant, designed to be valid for both commercial and open-source use while disclaiming all liability. It also contains important philosophical principles regarding F.C. Barcelona, fine wine, and the wisdom of Shakira. It is recommended you read it for both legal clarity and entertainment.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue.
