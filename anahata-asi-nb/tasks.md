# Tasks for Anahata ASI NetBeans (`anahata-asi-nb`)

## High Priority
- [x] **[UI] UI & Context Synchronization**: Ensured the **Context Tab** (Resources node) is fully reactive by removing child caching in the tree nodes. File icons are correctly restored after session reboot via the `NbFileObjectResourceHelper`. Fixed the `JXTreeTable` column jumping issue by disabling auto-creation of columns.
- [x] **[MULTIMODAL] Binary Resource Support**: Implemented `NbBinaryFileResource` and `EditorCookie` detection.
- [x] **[UI] Surgical vs. Hierarchical Badging**: Implemented view detection to keep Projects tab clean while allowing bubbling in the Files tab.
- [x] **[UI] Tooltip Deduplication & Branding**: Fixed the Git double-tooltip issue and added Anahata branding.
- [x] **[CORE] Session Persistence**: Fixed the Kryo hidden-class crash by refactoring the resource helper.
- [x] **[UI] V2 AgiNodeFactory**: Ported the V1 logic to create the virtual "Anahata" folder in the Projects tab and handle root `.md` files. Standardized on the "Anahata" name for brand consistency.

## UI Refinements (Completed)
- [x] Update context menu labels to use "Session" instead of "Chat".
- [x] Fix "null" nickname display in context menus.
- [x] Implement branded "Add/Remove from Context" icons.
- [x] Fix recursive add/remove confusion (now non-recursive by default from menu).
- [x] Fix folder addition logic (now adds immediate files even if non-recursive).
- [x] **Smart File Labels**: Only show `[SessionName]` or `[n]` if multiple sessions are active.
- [x] **Sorted Folder Labels**: Ensure `[n][m]` labels are sorted by session name for consistency.

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
