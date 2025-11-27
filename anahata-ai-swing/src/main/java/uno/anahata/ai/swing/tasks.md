# Anahata AI Swing UI - Task List

This document outlines the development plan for the V2 Swing UI, as per the latest strategic directives.

## Phase 1: UI Skeleton & Foundation (In Progress)

1.  **`ChatPanel` Creation:** Create the main `ChatPanel.java` class as the central aggregator for all UI components.
2.  **`ToolsPanel` Creation:** Create the new `ToolsPanel.java` (formerly `FunctionsPanel`).
3.  **Tabbed Interface:** Integrate the `ChatPanel` and `ToolsPanel` into a `JTabbedPane` within the main `JFrame` in `SwingMain`.

## Phase 2: Component Implementation

1.  **Toolbar:** Implement the left-hand toolbar with "Clear Chat," "Toggle Functions," and "Toggle Autoreply" actions.
2.  **Top Bar:** Implement the provider and model selection combo boxes.
3.  **`ToolsPanel` Details:** Flesh out the `ToolsPanel` to display detailed information about V2 toolkits, including descriptions, parameters, schemas, and logging capabilities.

## Phase 3: Future Enhancements (Deferred)

-   **Input Panel:** Enhance the input panel to show attachments and a live preview.
-   **Live Workspace:** Implement the functionality for the "Live Workspace" button.
-   **Session Management:** Implement the "Save" and "Load Session" buttons.
