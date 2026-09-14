# Anahata ASI OpenAI Provider (`anahata-asi-openai`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

## 1. Purpose & Scope
This module is the canonical implementation for all OpenAI API protocols:
1. **OpenAI Responses API** (`uno.anahata.asi.openai`):
   - Modern, stateful, item-based protocol (`/v1/responses`).
   - Supports native server-side tools (Web Search, Code Interpreter).
   - Manages encrypted reasoning transmission for stateless clients and plain-text reasoning summaries for verified organizations.
2. **OpenAI Chat Completions Protocol** (`uno.anahata.asi.openai.compatible`):
   - Universal specification for standard `/v1/chat/completions`.
   - Base engine inherited by the "Universal Alliance" providers (Ollama, Mistral, NovaRouteAI, OpenRouter, Nvidia, HuggingFace, Modal).
   - Supports pluggable reasoning extraction strategies (FIELD, TAGS, NONE).

## 2. Dependencies
- Depends on `anahata-asi-core`.
- Serves as the upstream dependency for `anahata-asi-openai-compatible`.

Força Barça!
