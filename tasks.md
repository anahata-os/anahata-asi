# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 1. Active Focus: Context Tree UI Refinement (Task 3)
*Goal: Ensure the hierarchical context tree is robust, informative, and visually consistent for both IDE and Standalone modes.*

- [x] **Display Names**: Fixed `toString()` in `AbstractContextNode` to ensure nodes display domain names instead of class names.
- [x] **Schema Inspection**: Implemented `ToolPanel` tabbed view for JSON schemas (Arguments, Return Type) and Native Declarations. Parameters are now shown in individual sections.
- [x] **Toolkit Branding**: Restored the V1 `java.png` icon for all toolkits in the tree.
- [x] **Context Provider Branding**: Added `javadoc.png` icon for context providers.
- [x] **History Branding**: Added `email.png` (envelope) icon for messages.
- [x] **UI Layout Stability**: Set minimum sizes for detail panels to prevent squeezing the tree view.
- [ ] **Startup & Session Sync**: Ensure the tree and detail panels refresh correctly upon startup and when switching sessions.
- [ ] **Token Snapshot**: Refine the "Refresh Tokens" background task to be non-blocking and reactive.
- [ ] **Vector Icon Port**: Copy the new `ThemedIcon` system and functional icons from `gemini-java-client` 1.0.14 to ensure crisp, theme-aware UI in V2.
- [ ] **Context Panel Fixes**:
    - [ ] **Column Sizing**: Fix the 'Name' column in the Context panel being too small on startup.
    - [ ] **Turns Left Visibility**: Investigate and fix why 'turns left' are not showing for tool calls in the context tree.

## 2. NetBeans Integration (V2)
*These tasks are focused on bringing the deep IDE agency of V1 to the V2 architecture.*

- [ ] **NetBeans File System Integration (Task 1)**:
    - [ ] `NbTextFileResource` with `FileChangeListener` for path/content sync.
    - [ ] `NbFiles` toolkit using `EditorCookie` for atomic, undoable writes.
    - [ ] Local History integration via change messages.
- [ ] **Enhanced IDE Visual Cues (Task 2)**:
    - [ ] Dynamic chat count labels `[n]` in the Projects tab.
    - [ ] Package-level aggregation of chat counts.
    - [ ] Icon badging (Anahata logo) for active projects.
- [ ] **Context Menu Improvements (Task 4)**:
    - [ ] File-level support for "Add/Remove from AI Context".
    - [ ] Multi-targeting: "Add to all active chats" and "Create new chat...".
- [ ] **AgiNodeFactory**: Port the V1 logic to create the virtual "Anahata" folder in the Projects tab and handle root `.md` files.
- [ ] **Project Alerts Refinement**:
    - [ ] **JavacAlerts Default**: Enable JavacAlerts by default (currently disabled).

## 3. Future Tactical Goals

- [ ] **Global Preferences Panel**: Create a centralized UI for managing global Anahata settings (API keys, theme, pruning policies).
- [ ] **Surgical Edits with Cherry-Picking**: Implement a V2 version of the `suggestChange` diff dialog using `org.netbeans.api.diff.DiffView`.
- [ ] **High-Performance Project Alerts**: Re-implement project alerts using background-driven `ErrorProvider` APIs.
- [ ] **Next-Gen Project Overview**: Explore Java Type-Based overviews and UML-like structural representations.
- [ ] **Context Window Optimization**: Implement proactive pruning strategies to keep usage below 80%.
- [ ] **CLI Module Revitalization**: Update the CLI to support the V2 domain model and tool execution flow.
