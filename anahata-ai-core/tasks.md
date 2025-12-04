# Anahata AI V2 - Actionable Task List

This document outlines the current, actionable tasks for the V2 development cycle, derived from the strategic objectives in `v2-commanders-briefing.md`.

## High Priority

-   [x] **Task 1: Implement `StatusManager` Listeners**
    -   **File:** `anahata-ai-core/src/main/java/uno/anahata/ai/status/StatusManager.java`
    -   **Objective:** Implement a robust listener mechanism for the `StatusManager` to allow different UI components or services to react to all `ChatStatus` changes (e.g., `IDLE`, `API_CALL_IN_PROGRESS`). The implementation should be based on the battle-tested patterns from the V1 codebase.
    -   **Status:** Completed.

-   [x] **Task 2: Achieve V1 Feature Parity in V2 `ToolManager`**
    -   **Objective:** The V2 `ToolManager` is already implemented. The objective is to perform a gap analysis against the V1 `ToolManager` from the `gemini-java-client` asset. Identify and migrate any missing features, critical logic (e.g., advanced error handling, specific tool call processing), or subtle behaviors to ensure the V2 implementation is as robust and feature-complete as its predecessor.
    -   **Status:** Completed.

-   [ ] **Task 4: Implement FailureTracker in V2 ToolManager**
    -   **Objective:** Integrate the existing `FailureTracker` class into the V2 `ToolManager` to track and temporarily block repeatedly failing tools.

-   [ ] **Task 5: Implement Asynchronous Job Execution for V2 Tools**
    -   **Objective:** Add support for asynchronous tool execution in the V2 framework, allowing long-running tasks to execute in the background without blocking the main chat flow.

## Research & Optimization

-   [ ] **Task 6: Research Shared Schema Definitions for Token Optimization**
    -   **Objective:** Explore the feasibility of creating a shared repository of common parameter and response schemas. Investigate if tool definitions can reference these shared schemas (e.g., by FQN) instead of including the full schema inline, with the goal of significantly reducing the token count in prompts sent to the model.

-   [ ] **Task 7: Improve Binary File Handling in `LocalFiles` Tool**
    -   **Objective:** The `LocalFiles.copyFile` tool currently fails on binary files due to character encoding issues. Investigate and implement a robust solution for handling binary data.
    -   **Note:** Current workarounds include using `RunningJVM.compileAndExecuteJava` to perform a binary copy with `java.nio.Files`, or using `LocalShell.runShell` with the `cp` command.

## Housekeeping

-   [x] **Task 3: Cleanup Redundant Documentation**
    -   **File:** `anahata-ai-core/v2-core-architecture.md`
    -   **Objective:** Delete this file. Its content has been successfully consolidated into `anahata-ai-core/anahata.md`.
    -   **Status:** Completed.
