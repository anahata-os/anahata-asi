/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI NetBeans (`anahata-asi-nb`)

This is the V2 NetBeans integration module for the Anahata ASI framework.

## 1. Core Principles

1.  **IDE API Preference**: Always prefer NetBeans APIs (e.g., `MimeLookup`, `EditorKit`) over direct file manipulation or generic Swing components when integrated into the IDE.

## 2. Key Components

-   **`AsiTopComponent`**: The main entry point for the JASI UI in NetBeans.
-   **`NetBeansEditorKitProvider`**: Provides NetBeans-specific `EditorKit`s for syntax highlighting in the chat UI.
-   **`NetBeansModuleUtils`**: Utilities for interacting with the NetBeans module system and classloaders.

## 3. Dependency Management

-   **`commons-io` Isolation**: This module explicitly bundles `commons-io` to avoid conflicts with the version used by the NetBeans Maven Embedder.
