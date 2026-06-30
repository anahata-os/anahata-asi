/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI IntelliJ IDEA (`anahata-asi-intellij`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides the IntelliJ IDEA plugin integration for the Anahata ASI framework.

## 1. Core Principles
1. **Swing UI Reuse**: IntelliJ uses a customized Swing implementation (similar to NetBeans and FlatLaf). The `anahata-asi-swing` components (`AgiPanel`, etc.) should be wrapped within an IntelliJ `ToolWindow`.
2. **IntelliJ Open API**: Leverage IntelliJ's OpenAPI for file access, editor manipulation, and project structure context.
