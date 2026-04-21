/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Modal Provider (`anahata-asi-modal`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides a pre-configured provider for Modal's GLM-5 inference endpoint.

## 1. Purpose

This module's sole responsibility is to act as a **pre-configured adapter** for Modal's GLM-5 API, extending `OpenAiCompatibleProvider` with sensible defaults.

## 2. Key Features

- **GLM-5 Model**: High-performance inference with native function calling support
- **Reasoning Support**: The API returns reasoning content in a dedicated `reasoning_content` field, which is automatically detected by `OpenAiModel` autodetection logic
- **OpenAI-Compatible**: Uses the standard OpenAI Chat Completion API format

## 3. API Key Acquisition

Get your API key at: https://modal.com/glm-5-endpoint

Força Barça!