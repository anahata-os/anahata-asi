# Tasks for Anahata ASI NetBeans (`anahata-asi-nb`)

## High Priority
- [ ] **[ACTIVE] Task 1: UI & Context Synchronization**: Ensure the **Context Tab** (Resources node) is fully reactive (updates immediately on add/remove) and that file icons are correctly restored after a session reboot.
- [x] **[MULTIMODAL] Binary Resource Support**: Implemented `NbBinaryFileResource` and `EditorCookie` detection.
- [x] **[UI] Surgical vs. Hierarchical Badging**: Implemented view detection to keep Projects tab clean while allowing bubbling in the Files tab.
- [x] **[UI] Tooltip Deduplication & Branding**: Fixed the Git double-tooltip issue and added Anahata branding.
- [x] **[CORE] Session Persistence**: Fixed the Kryo hidden-class crash by refactoring the resource helper.

## Structural IDE Features (Inactive)
- [ ] Implement V2 `AgiNodeFactory`: Port the V1 logic to create the virtual "Anahata" folder in the Projects tab and handle root `.md` files.

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

## Future Exploration
- [ ] **Local History Integration**: Integrate with the IDE's local history for better context tracking.
