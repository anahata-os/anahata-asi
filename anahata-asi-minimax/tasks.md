# MiniMax Provider Integration Tasks

## Provider Redirection
- [ ] Refactor `anahata-asi-minimax` to extend from the newly created `AnthropicProvider` instead of `OpenAiCompatibleProvider` (as recommended by their docs for optimal feature support).

## Model Inspection
- [ ] Determine the behavior of MiniMax's `/models` endpoint to see if dynamic model discovery is possible, or if models need to be hardcoded like Anthropic and Google Vertex Express.

## Advanced Features
- [ ] Investigate how to inject and retrieve the proprietary `audio_content` multimodal blocks.
- [ ] Investigate `<think>` tag parsing if it is officially supported in the Anthropic-compatible mode.