# Tasks for Anahata ASI NetBeans (`anahata-asi-nb`)

## High Priority
- [ ] **Investigate Double Tooltips**: Determine if the doubled project tooltips are caused by V1/V2 plugin conflicts or redundant annotator calls.
- [ ] **Augment Project Tooltips**: Enhance the project node tooltips in the Projects tab to include AI-related information (e.g., which sessions have the project in context).
- [ ] **Annotate Project Names**: Explore ways to annotate the project name (e.g., adding a suffix or changing color) when it's active in a session.
- [ ] **Programmatic Annotation Extraction**: Explore `AnnotationProcessingQueryImplementation` to programmatically retrieve all annotations and tooltip content for a project to include in `ProjectContextProvider`.

## Structural IDE Features
- [ ] **Implement V2 `AgiNodeFactory`**: Port the V1 logic to create the virtual "Anahata" folder in the Projects tab and handle root `.md` files. Use the name `AgiNodeFactory` to avoid clashes.

## UI Refinements
- [x] Update context menu labels to use "Session" instead of "Chat".
- [x] Fix "null" nickname display in context menus.
- [x] Implement branded "Add/Remove from Context" icons.
- [x] Fix recursive add/remove confusion (now non-recursive by default from menu).

## Engineering Notes & Rationale
- **`Rebindable` Interface**:
    - **Purpose**: Restores transient state (listeners, locks, external handles) that Kryo cannot serialize.
    - **Execution Timing**: `rebind()` is called by `RebindableWrapperSerializer` after the object is read but *potentially before* all other objects in a circular graph are fully initialized.
    - **Circular Dependency Guards**: Always use null guards (e.g., `if (tools == null)`) in `rebind()` and getters to handle cases where Kryo's reference map contains a partially-initialized object during restoration.
- **`JavaObjectToolkit` Handshake**:
    - **Rationale**: The `tc.setToolkit(this)` call ensures that the logic instance (the `@AiToolkit` class) always has a valid reference to its metadata wrapper, which is essential for the `ToolContext` API (logging, chat access) to function after a session is restored or the plugin is reloaded.
- **Annotator Handshake (`uno.anahata.asi.badged`)**:
    - **Rationale**: A programmatic flag set on a `FileObject` attribute. It allows `AnahataProjectAnnotator` to signal to `FileAnnotationProvider` that a node has already been badged, preventing redundant "double icons" on project roots without relying on fragile tooltip string checks.

## Future Exploration
- [ ] **Local History Integration**: Integrate with the IDE's local history for better context tracking.
