# Anahata AI Swing UI - Task List

This document outlines the development plan for the V2 Swing UI, as per the latest strategic directives.

## Phase 1: UI Skeleton & Foundation (Completed)

-   [x] **`ChatPanel` Creation:** Create the main `ChatPanel.java` class as the central aggregator for all UI components.
-   [x] **`ToolsPanel` Creation:** Create the new `ToolsPanel.java` (formerly `FunctionsPanel`).
-   [x] **Tabbed Interface:** Integrate the `ChatPanel` and `ToolsPanel` into a `JTabbedPane` within the main `JFrame` in `SwingMain`.

## Phase 2: Component Implementation (In Progress)

1.  **Toolbar:** Implement the left-hand toolbar with "Clear Chat," "Toggle Functions," and "Toggle Autoreply" actions.
2.  **Top Bar:** Implement the provider and model selection combo boxes.
3.  **`ToolsPanel` Details:** Flesh out the `ToolsPanel` to display detailed information about V2 toolkits, including descriptions, parameters, schemas, and logging capabilities.
4.  **Advanced `InputPanel` Implementation:**
    -   **Objective:** Replace the placeholder `InputPanel` with a fully featured, interactive component.
    -   **Sub-tasks:**
        -   [ ] **Component Upgrade:** Use a `JXTitledPanel` and `JXTextArea` from the SwingX library.
        -   [ ] **Live Preview:** Implement a side panel that renders a live preview of markdown content and attachments.
        -   [ ] **Dynamic `UserMessage`:** The panel must manage a `UserMessage` object in real-time. Text changes update its `TextPart`, and file drops add `BlobPart`s.
        -   [ ] **Stop Button:** Add a "Stop" button that is only enabled when the chat status is busy.
        -   [ ] **Send/Queue Logic:** The send button must send the composed `UserMessage`. If the chat is busy, the message should be added to an outgoing queue.
        -   [ ] **Generic SwingWorker:** Create a reusable utility for background Swing tasks.

## Phase 3: Future Enhancements (Deferred)

-   **Live Workspace:** Implement the functionality for the "Live Workspace" button.
-   **Session Management:** Implement the "Save" and "Load Session" buttons.
