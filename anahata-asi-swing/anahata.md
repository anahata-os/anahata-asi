/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Swing UI (`anahata-asi-swing`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides the reusable, provider-agnostic Swing UI components for building agentic workflows.

## 1. Key Components

-   **`AgiPanel`**: The main, top-level panel aggregator for a session.
-   **`ConversationPanel`**: Manages the list of message panels.
-   **`AbstractMessagePanel`**: Base class for rendering messages.
-   **`AbstractPartPanel`**: Base class for rendering individual message parts.
-   **`TextPartPanel`**: Renders markdown text and code blocks.

## 2. UI Rendering Strategy

We use a "Diff-and-Update" strategy for rendering the conversation:
-   **Message Level**: `ConversationPanel` tracks `AbstractMessage` objects and reuses `AbstractMessagePanel` instances.
-   **Part Level**: `AbstractMessagePanel` tracks `AbstractPart` objects and reuses `AbstractPartPanel` instances.
-   **Text Segment Level**: `TextPartPanel` parses markdown into `TextSegmentDescriptor`s and reuses specialized renderers.

This ensures that even in very long conversations, the UI remains responsive and memory-efficient.

## 3. Theming & Host-Aware Configuration (`SwingAgiConfig`)

1. **Instance-Based Architecture**: All UI theming, warning colors, and toolbar button styling reside as overridable instance methods on `SwingAgiConfig`.
2. **`AgiPanel.getConfig()` Standard**: Swing UI components must never check `instanceof SwingAgiConfig` or perform redundant casting. All components obtain the active Swing configuration directly via `agiPanel.getConfig()`.
3. **Pluggable Host Fidelity**: Host IDE implementations (`IntellijAgiConfig`, `NetBeansAgiConfig`, `AsiDesktopAgiConfig`) override these instance methods to provide native platform colors (e.g. IntelliJ New UI palettes, FlatLaf theme tokens) and platform-specific toolbar button styling without dirty static hacks.
