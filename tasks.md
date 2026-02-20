# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 1. Active Focus: Context Tree UI Refinement (Task 3)
*Goal: Ensure the hierarchical context tree is robust, informative, and visually consistent for both IDE and Standalone modes.*

- [] **UI Performance**: Terrible unusable UI performance after 40 messages or so, had to uninstall v2, seems that either everything re-renders fully on every turn or we messed up the SwingUtils.runAndWaitOnEDT, EDT totally irresponsive for up to 20 seconds or more. Consider implementing a slowness detector that reports EDT unavailable for more than 1 second

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
- [ ] **Editor Integration Review**:
    - [ ] Review if using one `NetBeansEditorKitProvider` instance per session makes sense, or if we should use a single static instance for all sessions.

## 3. Future Tactical Goals

- [ ] **V1 to V2 Migration**: Implement a check on V2 startup (especially if the `~/.anahata/asi` directory is missing) to detect V1 API keys in `~/.anahata/ai-assistant/gemini_api_keys.txt` and offer to copy them to the V2 directory.
- [ ] **Hierarchical Agent Management**:
    - [ ] **Subagent API**: Design a new API for the model to spawn subagents with fine-grained control over `ChatConfig` (provider, model, toolkits, permissions).
    - [ ] **Reporting Mechanism**: Implement a way for subagents to report task completion and results back to the "Boss" agent.
    - [ ] **Parent/Child Chats**: Establish a formal parent-child relationship between `Chat` instances to support complex agentic hierarchies.
- [ ] **Domain Model Refinement**: Discuss and potentially rename `Chat.java` to something more descriptive like `Session.java`, `Agent.java`, or `Agi.java`.
- [ ] **CwQL**: Create a Context Window Query Language spec and implementation. So if the model spawans subagents or wants to peek into saved or disposed sessions. A simple query language can be used like 
        - sessionUUID/history(role=model)/partType=text/thought=false 
        - sessionUUID/tools/RadioTool/selectedPlaybackDevice (to look up the selectedPlaybackDevice field in the RadioTool) 
        - sessionUUID/status or sessionUUID/history/size 
        - disposed/sessionUUID/history/role=model/(matching:'Task completed')
        - remoteContaier/*(all sessions)/history/role=model/(matching:'Task completed')
        - or anything that would allow the ASI to surgically check what other agents are doing or what is in the saved or dispossed sessions dir (infinte memory)
- [ ] **Global Preferences Panel**: Create a centralized UI for managing global Anahata settings (API keys, theme, pruning policies).
- [x] **Surgical Edits with Cherry-Picking**: Implemented a V2 version of the `updateTextFile` diff dialog using `org.netbeans.api.diff.DiffController` and a custom cherry-picking UI.
- [ ] **High-Performance Project Alerts**: Re-implement project alerts using background-driven `ErrorProvider` APIs.
- [ ] **Next-Gen Project Overview**: Explore Java Type-Based overviews and UML-like structural representations.
- [ ] **Context Window Optimization**: Implement proactive pruning strategies to keep usage below 80%.
- [ ] **CLI Module Revitalization**: Update the CLI to support the V2 domain model and tool execution flow.
