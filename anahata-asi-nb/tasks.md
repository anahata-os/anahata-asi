# Tasks for Anahata ASI NetBeans (`anahata-asi-nb`)

## Strategy: Smart File Operations (In Progress)
- [ ] **[TOOLKIT] Consolidation to NbFiles**: Move all specialized refactoring and surgical edit logic from `NbCoding` to `NbFiles`. Override `replaceInMultipleTextFiles` to use NetBeans `DataObject` and `EditorCookie` APIs for memory-safe updates.
- [ ] **[UI] Inline Diffs**: Implement a dedicated "Diff" renderer for file update results. Show git-style diffs in the conversation panel when files are created or modified.
- [ ] **[CORE] Change IDs**: Implement unique Change IDs for every file operation to improve traceability and feedback (format: `change-id: user-description`).
- [ ] **[UI] Embedded NetBeans Diff**: Explore embedding the native NetBeans Diff viewer in the `ToolCallPanel` arguments tab for `writeTextFile` operations.

## UI Refinements (High Priority)
- [ ] **[UI] Dynamic Tab Height**: Ensure `argsTabbedPane` and `resultsTabbedPane` in `ToolCallPanel` resize vertically to fit the selected tab's content.
- [ ] **[UI] JEditorPane Height**: Fix the persistent white-space issue in NetBeans when rendering `JEditorPane` inside tool arguments.
- [ ] **[UI] Default Line Numbers**: Enable line numbers by default in the text file resource viewport and add Git annotations.


## Engineering Notes & Rationale
- **Context Menu Registration**: Use explicit MIME-type registrations (`text/x-java`, `text/x-maven-pom+xml`) and include `image/any` and `video/any` for multimodal support.
- **`Rebindable` Interface**: Restores transient state (listeners, locks, external handles) that Kryo cannot serialize. `rebind()` is called by `RebindableWrapperSerializer` after the object is read.
- **Annotator Handshake (`uno.anahata.asi.badged`)**: A programmatic flag set on a `FileObject` attribute. It allows `AnahataProjectIconAnnotator` to signal to `FileAnnotationProvider` that a node has already been badged.
- **Column Persistence**: `treeTable.setAutoCreateColumnsFromModel(false)` is critical for preserving user-resized columns during model refreshes.
- **Conceptual Taxonomy**:
    - **Java Introspection**: Low-level, reflection-like exploration of types and members (V1 `JavaIntrospection`).
    - **Java Analysis**: High-level semantic analysis, usage tracking, and refactoring impact (V1 `JavaAnalysis`).
    - **Java Code Model**: A stable, polymorphic representation of the codebase (The "Keychain" pattern) designed for cross-turn agentic reasoning (V2 `CodeModel`).

## Future Exploration
- [ ] **Local History Integration**: Integrate with the IDE's local history for better context tracking.
- [ ] **Code Model Consolidation**:
    - [ ] Evaluate V1 `findTypesInPackage` for migration to V2 `CodeModel`.
    - [ ] Compare V1 `JavaIntrospection.getMembers` with V2 `CodeModel.getMembers` to determine the superior implementation for agentic use.
- [ ] **Dependency Audit**: Review all `impl` module dependencies in `pom.xml`. Many were added to simplify runtime library management but they break cross-version compatibility. Replace with `spec` dependencies where possible.
- [ ] **EditorKitProvider Review**: Investigate if `NetBeansEditorKitProvider` should be a static singleton instead of being instantiated per session in `NetBeansChatConfig`.

- [ ] Enhance `Projects.java` with `setCompileOnSave(String projectId, boolean enabled)` to allow programmatic control of the IDE's Compile on Save feature.
- [ ] **[TOOLKIT] System Toolkit**: Create a dedicated `System` toolkit (or enhance `Shell`) to house general-purpose tools like `killProcess(pid)`.
