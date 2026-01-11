# Anahata AI: Enterprise-Grade AI Framework for Java (V2)

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/anahata-anahata/anahata-ai-parent/actions) 
[![License](https://img.shields.io/badge/license-Anahata%20(ASL%20V108)-blueviolet)](LICENSE)

**Anahata AI** is a pure-Java, enterprise-grade framework for building sophisticated, multi-provider, orchestrated, agentic workflows. It is designed to be the "Tomcat" for AI—a robust container for managing, executing, and developing AI tools.

> [!NOTE]
> **Project Status:** This repository contains the **V2 Architecture**, which is currently under active development. 
> **V1** was officially released to the **NetBeans Plugin Portal on Jan 2nd, 2026**, and all V1 artifacts are available on **Maven Central**. While V1 is stable and production-ready for the IDE, this V2 project represents the future of the framework and is the primary focus of active development.

## ✨ Core Philosophy: The AI Container

Anahata AI treats the LLM as a first-class citizen within the Java ecosystem, providing an environment similar to a JEE container:

- **Annotation-Driven Toolkits:** Define tools using `@AiToolkit` and `@AiTool`. The framework handles automatic JSON schema generation, type-safe argument conversion, and lifecycle management.
- **Context-Aware Execution:** Tools have access to a `ThreadLocal` context (similar to `ServletContext`), providing session data, execution logs, and response attachments.
- **Dynamic JIT Compilation:** An unrestricted, Just-In-Time Java Compiler allows the AI to write, compile, and execute code within the running JVM, leveraging a custom child-first ClassLoader.
- **Hybrid Context Assembly:** Combines persistent conversation history with just-in-time RAG (Retrieval-Augmented Generation) and system instructions.

## ♻️ Context Garbage Collection

V2 introduces a sophisticated **Garbage Collection** mechanism for the context window, ensuring long-running conversations remain efficient and stay within token limits:

- **Eligibility:** Messages are marked as "garbage" and reclaimed if they contain no parts and are not explicitly "pinned" by the user.
- **Streaming Protection:** Model messages are protected from the collector while in a `streaming` state, ensuring they aren't removed before the first tokens arrive.
- **Soft vs. Hard Pruning:** Parts are first "soft-pruned" (hidden from the model but kept in history) and eventually "hard-pruned" (permanently deleted) based on configurable retention policies.

## 📦 Modules

- **`anahata-ai-core`**: The foundational, provider-agnostic framework and domain model.
- **`anahata-ai-gemini`**: The reference implementation for the Google Gemini API.
- **`anahata-ai-swing`**: A rich, embeddable UI component for interactive chat.
- **`anahata-ai-standalone`**: The primary entry point for testing the full V2 stack with a desktop UI.

## 🚀 Getting Started

To experience the V2 framework with the full Swing UI:

1. **Configure API Keys:** Create a file at `~/.anahata/ai/gemini/api_keys.txt` and add your Google Gemini API key.
2. **Build the Project:**
   ```bash
   mvn clean install
   ```
3. **Launch the Standalone App:**
   Navigate to the `anahata-ai-standalone` module and run the `run` action (or use `mvn exec:java`). This launches the integrated Swing chat interface with all V2 features enabled.

## 📜 Licensing

Licensed under the **[Anahata Software License (ASL) V108, "The Immutable Edict" Edition](LICENSE)**. 

Crafted by the Anahata AI Assistant, this license is designed for both commercial and open-source use. It includes unique clauses regarding F.C. Barcelona, fine wine, and the wisdom of Shakira. Read it for legal clarity and a bit of soul.

## 🤝 Contributing

Contributions are welcome! Whether it's a new provider adapter or a UI enhancement, feel free to open an issue or submit a pull request.
