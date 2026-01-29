# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 1. High Priority (Active Development)

- [ ] **NetBeans File System Integration**: Replace direct disk I/O in `anahata-asi-nb` with NetBeans FileSystem APIs.
    - [ ] Create `NbTextFileResource` extending `TextFileResource` to wrap `FileObject`.
    - [ ] Implement `FileChangeListener` in `NbTextFileResource` to keep context in sync with IDE changes (moves, deletes).
    - [ ] Create `NbFiles` toolkit to replace/complement core `Files` toolkit, using `DataObject` and `EditorCookie` for writes.
    - [ ] Ensure all file modifications trigger NetBeans' local history and respect IDE file locks.
- [ ] **Enhanced IDE Visual Cues**: Improve the visibility of AI-managed resources within the NetBeans UI.
    - [ ] Implement file-level badges (Anahata logo) for files currently in an active AI context.
    - [ ] Investigate and refine `AnahataAnnotationProvider` for consistent resource decoration across different explorer views.
    - [ ] **Git Integration**: Mark line numbers in resources with Git changes (diff status) and include Git status in the project overview file tree.
- [ ] **Advanced Context Targeting**: Refine the "Add to Context" / "Remove from Context" workflow.
    - [ ] **Recursive Folder Support**: Update context actions to recursively add/remove all files within selected packages or folders.
    - [ ] **Remove from Context**: Implement the counterpart action to remove files/folders from a specific chat or all chats.
    - [ ] **Multi-Chat Targeting**: Add "Add to all active chats" and "Add to new chat..." options to the context menu.
- [ ] **Surgical Edits with Cherry-Picking**: Implement a V2 version of the `suggestChange` diff dialog.
    - [ ] Create a `DiffDialog` utility in `anahata-asi-nb` that wraps `org.netbeans.api.diff.DiffView`.
    - [ ] Integrate this dialog into `replaceInFile` and `writeTextFile` (optional) to allow users to "cherry-pick" changes.
    - [ ] Ensure the dialog supports manual edits in the "Proposed" pane before saving.
- [ ] **High-Performance Project Alerts**: Re-implement project alerts (Javac errors, project problems) in V2.
    - [ ] Explore the NetBeans `ErrorProvider` APIs for a more efficient, background-driven approach to avoid UI freezes.
- [x] **Unified Context Provider Architecture**: Refactor `AbstractResource` and `ResourceManager` to implement `ContextProvider`.
- [x] **Hierarchical Context Tree**: Implement a JNDI-style tree view in the Swing UI for managing the entire AI context.
- [x] **Authentic IDE Icons**: Integrate NetBeans project and file icons into the Swing context tree using a decoupled icon registry.
- [ ] **Context Window Optimization**: Implement proactive pruning strategies to keep the context window usage below 80%.

## 2. Medium Priority

- [ ] **Next-Gen Project Overview**:
    - [ ] Explore "Java Type-Based" overviews (listing classes/interfaces/inner classes) vs. traditional "FileSystem-Based" trees.
    - [ ] Implement configurable overview depth: `includeTypes`, `includeMembers`, etc.
    - [ ] Add a context provider that generates a UML-like string representation of the entire project structure.
- [ ] **PowerShell Object-Oriented Tool**:
    - [ ] Explore creating a specialized `PowerShell` toolkit for Windows.
    - [ ] Leverage `ConvertTo-Json` to provide structured data to the AI, avoiding text scraping.
- [ ] **Provider-Specific Grounding**: Enhance the `GroundingMetadata` model to support provider-specific grounding sources (e.g., Google Search results).
- [ ] **Streaming UI Enhancements**: Improve the responsiveness of the `ConversationPanel` during high-throughput streaming.
- [ ] **CLI Module Revitalization**: Fix the `anahata-asi-cli` module to support the new V2 domain model and tool execution flow.

## 3. Low Priority / Future Ideas

- [ ] **Multi-Model Orchestration**: Explore strategies for using multiple models within a single chat session (e.g., a fast model for summarization, a powerful model for coding).
- [ ] **Voice Integration**: Improve the `MicrophonePanel` and integrate with high-quality STT/TTS providers.
- [ ] **Agentic Workflows**: Define a standard for multi-turn agentic workflows that can be shared across different host applications.
