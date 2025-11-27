# Anahata AI V2 - Actionable Task List

This document outlines the current, actionable tasks for the V2 development cycle, derived from the strategic objectives in `v2-commanders-briefing.md`.

## High Priority

-   [ ] **Task 1: Implement `StatusManager` Listeners**
    -   **File:** `anahata-ai-core/src/main/java/uno/anahata/ai/status/StatusManager.java`
    -   **Objective:** Implement a robust listener mechanism for the `StatusManager` to allow different UI components or services to react to all `ChatStatus` changes (e.g., `IDLE`, `API_CALL_IN_PROGRESS`). The implementation should be based on the battle-tested patterns from the V1 codebase.

-   [ ] **Task 2: Achieve V1 Feature Parity in V2 `ToolManager`**
    -   **Objective:** The V2 `ToolManager` is already implemented. The objective is to perform a gap analysis against the V1 `ToolManager` from the `gemini-java-client` asset. Identify and migrate any missing features, critical logic (e.g., advanced error handling, specific tool call processing), or subtle behaviors to ensure the V2 implementation is as robust and feature-complete as its predecessor.

## Housekeeping

-   [ ] **Task 3: Cleanup Redundant Documentation**
    -   **File:** `anahata-ai-core/v2-core-architecture.md`
    -   **Objective:** Delete this file. Its content has been successfully consolidated into `anahata-ai-core/anahata.md`.
