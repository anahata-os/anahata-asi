/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Swing UI (`anahata-asi-swing`)

This module provides the reusable, provider-agnostic Swing UI components for building agentic workflows.

## 1. Key Components

-   **`ChatPanel`**: The main, top-level panel aggregator.
-   **`ConversationPanel`**: Manages the list of message panels.
-   **`AbstractMessagePanel`**: Base class for rendering messages.
-   **`AbstractPartPanel`**: Base class for rendering individual message parts.
-   **`TextPartPanel`**: Renders markdown text and code blocks using specialized segment renderers.
-   **`AbstractCodeBlockSegmentRenderer`**: Base for code block rendering with support for editing and copying.

## 2. UI Rendering Strategy

We use a "Diff-and-Update" strategy for rendering the conversation:
-   **Message Level**: `ConversationPanel` tracks `AbstractMessage` objects and reuses `AbstractMessagePanel` instances.
-   **Part Level**: `AbstractMessagePanel` tracks `AbstractPart` objects and reuses `AbstractPartPanel` instances.
-   **Text Segment Level**: `TextPartPanel` parses markdown into `TextSegmentDescriptor`s and reuses `AbstractTextSegmentRenderer` instances (e.g., for code blocks).

This ensures that even in very long conversations, the UI remains responsive and memory-efficient.
