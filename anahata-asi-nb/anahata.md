/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI NetBeans (`anahata-asi-nb`)

This is the V2 NetBeans integration module for the Anahata ASI framework.

## 1. Core Principles

1.  **IDE API Preference**: Always prefer NetBeans APIs (e.g., `MimeLookup`, `EditorKit`) over direct file manipulation or generic Swing components when integrated into the IDE.

## 2. Key Components

-   **`AgiTopComponent`**: The main entry point for the JASI UI in NetBeans. It is a NetBeans UI wrapper for a `ChatPanel`.
-   **`NetBeansEditorKitProvider`**: Provides NetBeans-specific `EditorKit`s for syntax highlighting in the chat UI.
-   **`NetBeansModuleUtils`**: Utilities for interacting with the NetBeans module system and classloaders.

## 3. Annotator Strategy

1.  **Project Badges**: Handled exclusively by `AnahataProjectAnnotator`.
    -   Position: Offset 16 (to the right of the 16x16 icon).
    -   Condition: Only if at least one active chat has the project provider for that project "providing".
2.  **File Badges**: Handled by `FileAnnotationProvider`.
    -   Scope: Individual files only (skip project roots).
    -   Status: **Temporarily Disabled** to isolate project badge issues.

## 4. Dependency Management

-   **`commons-io` Isolation**: This module explicitly bundles `commons-io` to avoid conflicts with the version used by the NetBeans Maven Embedder.

## 5. Very Important Notes
- **Do not "clean" the project** to avoid deleting runtime JARs.
- **nbmreload** is the preferred way to test changes to tools or dependencies.
- **IMPORTANT (nbmreload & Context):** When you invoke `nbmreload`, the NetBeans module classloader disposes of all old classes. To preserve continuity, the current conversation context is automatically serialized to a Kryo file. After the reload, the new version of the plugin loads this saved context (using the same UUID) into a fresh `AgiTopComponent` instance. **You must totally stop your current task** before reloading, as the `Chat` instance is entirely replaced.
- **IMPORTANT (Module Dependencies):** This plugin depends on `anahata-asi-core`, `anahata-asi-swing`, and `anahata-asi-gemini`. An `nbmreload` on this project **does not** automatically build those dependencies. If you modify any of those modules, you **must** run `maven clean install` (or `build-with-dependencies`) on them before reloading the plugin.
- **IMPORTANT (Asynchronous Actions & Batching):** Invoking NetBeans supported actions (like `build-with-dependencies` or `nbmreload`) via the `Projects.invokeAction` tool is **asynchronous**. The tool returns immediately after firing the action. 
    - **CRITICAL:** Never batch multiple asynchronous actions that depend on each other (e.g., `build-with-dependencies` and `nbmreload`) in a single turn. They will execute concurrently, leading to build failures or inconsistent plugin states. You must trigger the build, wait for completion, and then trigger the reload in a subsequent turn.
- **Note on Dependency Warnings**: You may see a warning about `aopalliance:asm:jar:9.8` being missing from the local repository. This is a known issue in the current NetBeans release and is fixed in the next version. You can safely ignore this warning.

## Future Exploration
- **Local History Integration**: Explore using `VCSSystemProvider.VersioningSystem localHistory = VersioningManager.getInstance().getLocalHistory(file, !fo.isFolder());` to integrate with the IDE's local history.
