/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI IDE Abstractions (`anahata-asi-ide`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides the universal, IDE-agnostic abstractions, domain DTOs, resource handles, and base toolkits shared across host IDE integrations (`anahata-asi-nb`, `anahata-asi-intellij`, and future `anahata-asi-eclipse`).

## 1. Core Principles

1. **Strictly Zero False Abstractions & 100% Preservation of Native IDE Semantics**:
   - Never force NetBeans concepts onto IntelliJ or IntelliJ concepts onto NetBeans or Eclipse.
   - Only abstract genuine redundancies where both IDEs share the exact same semantics.
   - Preserve localized context and terminology so the AI model working in a specific IDE sees authentic, native idioms and conventions.
   - **Concrete Example / Cautionary Anti-Pattern (The Quick-Fix Execution Badges Leak)**:
     When implementing quick-fix execution badges (`(⚡)` for headless fixes runnable via `applyHint` vs `(👤)` for interactive UI dialogs/popups), this documentation belongs **strictly** in `IntellijHints.getSystemInstructions()`. Leaking `(⚡)` and `(👤)` into `AbstractHints.getSystemInstructions()` in `anahata-asi-ide` is a violation of this rule: NetBeans (`NbHints`) does not have `(⚡)`/`(👤)` badges because NetBeans applies fixes via `applyHintFix` using static rule catalog IDs, not line-targeted `IntentionAction`s. Host-specific tool capabilities, execution modes, and badges must never be placed in abstract base toolkits.

2. **No `@AgiTool` Annotations on Abstract Toolkit Methods**:
   - Do **NOT** put `@AgiTool` or `@AgiToolParam` annotations on abstract methods in abstract toolkits unless the semantics, parameter descriptions, and behavior are 100% identical letter-by-letter across all IDEs.
   - Concrete toolkits must own their `@AgiTool` and `@AgiToolParam` annotations, ensuring tool descriptions and parameter prompts reflect the exact mental model and semantics of that host IDE.

3. **Tool Return Types: String vs. DTO**:
   - If an `@AgiTool` method is consumed strictly by the AI model as prompt text (and not referenced or queried programmatically by internal framework code), declaring it to return a clean, token-efficient `String` (Markdown) rather than forcing an unused intermediate DTO is the correct and lean approach.
   - DTOs are reserved for data that the framework itself needs to programmatically consume (such as `HintInfo` for `IdeHandle.getAnnex()`, `VcsDiff`, `HistoryEntry`, or AST batch models).

4. **Shared `populateMessage` and `getSystemInstructions` in Abstract Toolkits**:
   - Abstract toolkits can provide default implementations of `populateMessage(RagMessage)` and `getSystemInstructions()` when the prompt augmentation or system instructions are universal across IDEs and can be derived by calling methods on the abstract toolkit.

5. **No Defensive Null Checks (Fail Fast)**:
   - In Anahata, foundational references (`owner`, `owner.getAgi()`, `project`, `fileObject`, `virtualFile`) are non-null by contract. Never check internal framework singletons or managed references for null. Let the JVM throw the `NullPointerException` immediately so root causes can be exposed and corrected.

