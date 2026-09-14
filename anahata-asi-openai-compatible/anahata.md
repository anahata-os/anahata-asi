# Anahata ASI OpenAI Compatible Providers (`anahata-asi-openai-compatible`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

## 1. Purpose & Scope
This module houses the **Universal Alliance** of third-party AI provider adapters that conform to the OpenAI Chat Completions specification:
- **`uno.anahata.asi.novarouteai`**: High-performance multi-provider aggregator.
- **`uno.anahata.asi.openrouter`**: OpenRouter gateway.
- **`uno.anahata.asi.ollama`**: Local Ollama server integration with model pulling and lifecycle management.
- **`uno.anahata.asi.mistral`**: Official Mistral AI endpoint.
- **`uno.anahata.asi.nvidia`**: NVIDIA NIM inference microservices.
- **`uno.anahata.asi.huggingface`**: Hugging Face Serverless Inference API.
- **`uno.anahata.asi.modal`**: Modal.com hosted models.

## 2. Architectural Relationship
- This module strictly depends on **`anahata-asi-openai`**, subclassing `OpenAiChatCompletionsProvider` and `OpenAiCompatibleModel` to add vendor-specific headers, URL routing, and reasoning styles.
- It contains zero protocol parsing logic of its own; all wire-level framing is inherited directly from `anahata-asi-openai`.

Força Barça!
