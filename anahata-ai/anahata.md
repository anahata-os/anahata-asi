# Anahata AI Core Framework

This document outlines the vision and architecture of the `anahata-ai` project, which serves as the core, model-agnostic AI framework.

## 1. Vision & Goal

The primary goal is to create a robust, extensible, and model-agnostic AI framework in Java. This core library defines a standard set of interfaces and a rich domain model for interacting with Large Language Models (LLMs), allowing developers to build AI-powered applications without being locked into a specific provider (e.g., Google Gemini, OpenAI, Anthropic).

This project contains the foundational logic, while provider-specific implementations are developed in separate "adapter" projects (e.g., `anahata-ai-gemini`).

## 2. Core Architecture

The framework is built on a sophisticated, decoupled architecture centered around a rich, model-agnostic domain.

### 2.1. Model Agnosticism (`uno.anahata.ai.model.*`)

The foundation of the framework is a set of abstract models that create a standardized language for all AI interactions.

-   **Provider Abstraction (`.provider`):**
    -   `AbstractAiProvider`: The key extension point for new AI providers. It uses a **two-phase initialization**: it is instantiated with a no-argument constructor, and then the `Chat` session is injected via an `init(Chat)` method.
    -   `AbstractModel`: A standard representation of an AI model's capabilities. In the V2 architecture, this class is now responsible for the core `generateContent(Request)` action, creating a more intuitive, object-oriented API.

-   **Core Conversation Model (`.core`):**
    -   `Request` & `Response`: Standardized, immutable objects for all generateContent calls.
    -   `AbstractMessage`: The base class for all messages, with type-safe subclasses `UserMessage`, `ModelMessage`, and `ToolMessage`.
    -   `AbstractPart`: The base for message content, allowing messages to be composed of multiple parts (e.g., `TextPart`, `AbstractToolCall`).

### 2.2. V2 Tool Framework (`uno.anahata.ai.tool` & `uno.anahata.ai.model.tool`)

The framework includes a powerful, reflection-based system for defining and executing tools.

-   **Annotation-Driven:** Tools are defined in plain Java using `@AiToolkit`, `@AiTool`, and `@AIToolParam` annotations.
-   **Rich Domain Model:** The framework parses annotated classes into a rich object model (`JavaObjectToolkit`, `JavaMethodTool`) that encapsulates all metadata, including Java `Method` and `Type` information.
-   **Type-Safe Execution:** A `ToolManager` orchestrates the tool lifecycle, using the rich domain model to perform validation and type-safe conversion of arguments from the model's JSON format into Java objects before execution.
-   **Automated Schema Generation:** A `SchemaProvider` uses reflection to generate detailed, OpenAPI 3-compliant JSON schemas for all tools, ensuring the model receives a precise definition of their capabilities.

### 2.3. Configuration (`uno.anahata.ai.config`)

Configuration is handled through a layered set of objects:

-   `AiConfig`: Manages global and application-specific settings, such as working directories.
-   `ChatConfig`: Holds the configuration for a single chat session, including the model ID and registered tool classes.
-   `Preferences`: A serializable POJO for persisting user settings (like tool permissions) to disk using the Kryo serializer.

### 2.4. Serialization (`uno.anahata.ai.internal.kryo`)

-   **Kryo:** The framework uses the high-performance Kryo library for serializing `Preferences` and entire chat sessions to disk. A thread-safe `KryoUtils` class manages Kryo instances.
