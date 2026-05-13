# Anthropic API Integration Tasks

## Core API Implementation
- [x] Create AnthropicProvider scaffolding.
- [x] Create AnthropicModel scaffolding.
- [x] Create AnthropicResponse scaffolding.
- [x] Create AnthropicMessage scaffolding.
- [x] Create AnthropicContentAdapter for converting Anahata messages to Anthropic format (`role: user` and `role: assistant`).
- [x] Implement JSON payload building for requests (System prompts, tools, messages).
- [x] Implement Synchronous (`generateContent`) HTTP calls using JDK HttpClient.
- [x] Implement Streaming (`generateContentStream`) HTTP calls with full Server-Sent Events (SSE) parsing for:
  - `message_start`
  - `content_block_start`
  - `content_block_delta`
  - `content_block_stop`
  - `message_delta`
- [x] Implement Tool Declaration Schema parsing specifically for Anthropic's format.

## Tool Calling Nuances
- [x] Ensure tool arguments buffer properly handles incremental JSON deltas during SSE streams.
- [x] Map `tool_use` blocks from the model into `JavaMethodToolCall`s.
- [x] Map executed `JavaMethodToolResponse` objects into `tool_result` blocks for the next turn.

## Model Support
- [x] Hardcode latest Anthropic models (`claude-3-5-sonnet-latest`, `claude-3-5-haiku-latest`, `claude-3-opus-latest`).

## Multimodal / Image Handling
- [x] Implement Base64 image attachment handling in `AnthropicContentAdapter`.