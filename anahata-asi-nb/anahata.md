/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI NetBeans (`anahata-asi-nb`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This is the V2 NetBeans integration module for the Anahata ASI framework.

## 1. Core Principles

1.  **IDE API Preference**: Always prefer NetBeans APIs (e.g., `MimeLookup`, `EditorKit`) over direct file manipulation or generic Swing components when integrated into the IDE.
2.  **Dependency Hygiene**: 
    - **Automatic Spec Dependencies**: All artifacts listed in the `<dependencies>` section of the `pom.xml` are automatically included as `spec` dependencies in the module manifest by the `nbm-maven-plugin`.
    - Version Alignment: Always ensure that library versions (especially `flexmark` and `jsoup`) match the versions bundled with the target NetBeans release.

## 2. Annotator Strategy

We use a non-intrusive annotation system to provide visual feedback and context within the NetBeans code editors and project views. This includes:
- **Project Icons & Names**: Real-time badges and session counters for Anahata-enabled projects.
- **Dynamic Context Menus**: Unified "AI Context" submenus across all file types via the `AnahataAnnotationProvider`.
- **Editor Annotations**: Real-time feedback from the ASI directly on the source code lines.

## 3. Dependency Management

-   **`commons-io` Isolation**: This module explicitly bundles `commons-io` to avoid conflicts with the version used by the NetBeans Maven Embedder.

## 4. Classpath Safety

> [!TIP]
> **Automated Classpath Safety**
> When using `NbJava.compileAndExecuteInProject`, the tool automatically detects the NBM packaging and filters out NetBeans Platform/Stub JARs to prevent `LinkageError`s.

## 5. Reloading and Lifecycle

- **nbmreload**: The preferred way to test changes to tools or dependencies.
- **CRITICAL**: Changing files in this project or its dependencies requires a manual `nbmreload` (or `build-with-dependencies` followed by `nbmreload`) for changes to take effect.
- **Turn Sequencing**: Never batch `nbmreload` with write operations. Wait for a successful compilation before reloading.

Força Barça!
