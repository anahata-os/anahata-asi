/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Gemini Live (Bidirectional GenerateContent) Integration Plan

This document outlines the architectural blueprint and implementation plan for integrating Gemini 3.1 Flash Live (Bidirectional WebSocket streaming) into the Anahata ASI framework.

---

## 1. Architectural Principles & Module Boundaries

The implementation strictly respects Domain Driven Architecture (DDA) and module boundaries across `anahata-asi-parent`:

```
+-----------------------------------------------------------------------------------+
|                                 anahata-asi-swing                                 |
|  - LiveSessionPanel (Voice/Video Toggle Bar in AgiPanel / HeaderPanel)           |
|  - Hardware Audio: Microphone recording & 24kHz speaker playback                  |
|  - Video Capture: java.awt.Robot captures NetBeans IDE window at 1 fps            |
+-----------------------------------------------------------------------------------+
                                          |
                                          v
+-----------------------------------------------------------------------------------+
|                                anahata-asi-gemini                                 |
|  - GeminiBidiWebSocketClient (Implements LiveSession)                             |
|  - Uses com.google.genai.types.LiveConnectConfig for setup & tool declarations    |
|  - 100% Headless! No Swing/GUI dependencies.                                      |
+-----------------------------------------------------------------------------------+
                                          |
                                          v
+-----------------------------------------------------------------------------------+
|                                 anahata-asi-core                                  |
|  - LiveSession interface (sendAudio, sendVideoFrame, sendText, sendToolResult)    |
|  - LiveSessionListener callbacks (onAudioOutput, onTextDelta, onToolCall)         |
|  - Headless Audio/AudioDevice utilities                                           |
+-----------------------------------------------------------------------------------+
```

---

## 2. Layer-by-Layer Detailed Design

### A. Core Module (`anahata-asi-core`)
1. **`uno.anahata.asi.agi.live.LiveSession`**:
   - Abstraction interface for full-duplex live sessions.
   - Methods: `sendAudioChunk(byte[] pcm16kData)`, `sendVideoFrame(byte[] jpegData)`, `sendText(String text)`, `sendToolResponse(String callId, Object result)`, `interrupt()`, `close()`.
2. **`uno.anahata.asi.agi.live.LiveSessionListener`**:
   - Callbacks for incoming server events: `onAudioOutput(byte[] pcm24kData)`, `onTextDelta(String textChunk)`, `onToolCall(String callId, String toolName, Map<String, Object> args)`, `onTurnComplete()`, `onError(Throwable t)`.
3. **`AbstractModel.openLiveSession(...)`**:
   - Polymorphic method added to `AbstractModel` returning a `LiveSession`.

### B. Gemini Provider Module (`anahata-asi-gemini`)
1. **`uno.anahata.asi.gemini.live.GeminiBidiWebSocketClient`**:
   - Connects to `wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1alpha.GenerativeService.BidiGenerateContent?key=...`.
   - Constructs the setup JSON payload using Google SDK's `com.google.genai.types.LiveConnectConfig` and `ContextWindowCompressionConfig`.
   - Sends real-time 16kHz PCM audio under `realtimeInput.audio` and 1 fps JPEG video frames under `realtimeInput.video`.
   - Parses incoming WebSocket binary frames containing UTF-8 JSON (`setupComplete`, `serverContent`, `inlineData`, `toolCall`).
   - Adapts `@AgiTool` method declarations via `GeminiFunctionDeclarationAdapter`.

### C. Swing UI Module (`anahata-asi-swing`)
1. **`uno.anahata.asi.swing.agi.live.LiveSessionPanel`**:
   - Interactive Swing component in `HeaderPanel` / `ToolbarPanel`.
   - Live visualizer bars for microphone input and speaker audio playback.
   - Microphone mute toggle for echo-cancellation during speaker playback.
2. **`SwingScreenStreamer`**:
   - Virtual thread capturing NetBeans IDE application window frames at 1 fps using `java.awt.Robot`, downscaling to max width 1024px, and JPEG-encoding.
3. **`ConversationPanel` Integration**:
   - Renders live streamed assistant text deltas as `ModelTextPart`s and tool execution requests as `ToolCallPanel`s in real-time.

---

## 3. Verified Technical Findings
- **Model Target**: `models/gemini-3.1-flash-live-preview`.
- **Token Limits**: 131,072 input tokens (128K) and 65,536 output tokens (64K).
- **Video Payload Schema**: Frames MUST be delivered under `realtimeInput.video` (`mimeType: "image/jpeg"`, `data: "<base64>"`). Using `media_chunks` is deprecated.
- **Binary Frame Decoding**: Google AI Studio delivers response envelopes as binary WebSocket frames containing UTF-8 JSON.
- **Echo Cancellation**: Automatically mute microphone stream while speaker line (`SourceDataLine`) is actively playing back 24kHz audio.

Força Barça!
