/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI IntelliJ IDEA (`anahata-asi-intellij`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

This module provides the IntelliJ IDEA plugin integration for the Anahata ASI framework. It is the IntelliJ sibling of `anahata-asi-nb` (NetBeans) and plugs into the same `anahata-asi-core` engine and `anahata-asi-swing` UI.

## 1. Core Principles
1. **Swing UI Reuse**: IntelliJ uses a customized Swing implementation (similar to NetBeans and FlatLaf). The `anahata-asi-swing` components (`AgiPanel`, `AsiCardsContainerPanel`) are wrapped within an IntelliJ `ToolWindow` via `AnahataToolWindowFactory`.
2. **IntelliJ Open API**: Leverage IntelliJ's OpenAPI (PSI, VFS, FileEditorManager, ProjectManager, refactoring) for all IDE integration. Never crawl internal UI classes or use reflection where a public API exists — the NetBeans plugin's internal-class/reflection hacks must NOT be ported.
3. **Module Leadership & Synchronization (Lead: Arslan)**: Arslan is the dedicated developer leading the `anahata-asi-intellij` integration. Any architectural updates, new methods, or contract changes to `anahata-asi-core` (such as `ResourceHandle.isModified()`), `anahata-asi-swing`, or `uno.anahata.asi.toolkit.resources` must be reflected here and synchronized across both IDE plugins.

## 2. Toolkit Registration
IDE toolkits are contributed by adding their `Class` to `getToolClasses()` in `IntellijAgiConfig` (mirrors `NetBeansAgiConfig`). There is **no** ServiceLoader / `@ServiceProvider` / classpath scanning: the `ToolManager` reflectively registers each listed class as a `JavaObjectToolkit`, and because every `AnahataToolkit` is also a `ContextProvider`, its context-provider subtree (its `childrenProviders`) is wired in automatically. Each toolkit therefore requires a **public no-arg constructor**; IDE handles (the active `Project`, etc.) must be resolved lazily, never injected.

## 3. Threading (the critical IntelliJ difference)
`@AgiTool` methods and `populateMessage` run on **background AI-execution threads**. The IntelliJ platform forbids touching PSI / editors / the window system off-thread. Conventions used throughout this module:
- **PSI reads** → wrap in `ReadAction.compute(() -> …)`. Never return live PSI across the read-action boundary; return DTOs/Strings.
- **PSI / document mutations** → `ApplicationManager.invokeAndWait(() -> WriteCommandAction.runWriteCommandAction(project, () -> …))` (single undoable command on the EDT).
- **Editor / tool-window / refactoring UI** → marshal onto the EDT with `ApplicationManager.invokeAndWait` (or `invokeLater` + latch for async builds).
- **Indexing (dumb mode)** → PSI/search tools call `JavaPsi.requireSmart(project)` (or `requireSmartForOpenProjects()`) first — a bounded `DumbService.waitForSmartMode` that throws a clean, retryable `AgiToolException` if still indexing. Context providers instead *skip* during indexing (`DumbService.isDumb`) rather than block the per-turn RAG. Both keep tools from throwing `IndexNotReadyException` or returning stale results. Async waits (builds, Maven goals) are bounded by a latch timeout, never indefinite.

## 4. Implemented Toolkits (parity with NetBeans)
| Toolkit | Package | Capability |
|---|---|---|
| `Projects` | `tools.project` | Open/close projects, structure + alerts context, `anahata.md` sync, `buildProject` (make/rebuild), `saveAllDocuments` |
| `CodeModel` | `tools.java` | Browse types/members/sources/javadocs, sub/supertype hierarchies (PSI, read-action wrapped) |
| `CodeRefiner` | `tools.java` | Structural imports (`addImports`/`optimizeImports`), `reformat`, `addAnnotation` (write-command) |
| `BatchCodeRefiner` | `tools.java` | V4 AST-guided batch splice: `refine` — insert/update/delete/move whole members atomically, returns a unified diff (PSI write-command) |
| `Editor` | `tools.ide` | `openFile`, `getOpenFiles`, `closeAllFiles` + caret/selection/visible-snippet context |
| `IDE` | `tools.ide` | `monitorLogs` (idea.log tail), `selectIn` (reveal in Project view), tool-windows report + context |
| `Refactor` | `tools.ide` | **complete** (14 tools): rename/renameMember/safeDelete/whereUsed/moveClass/moveMembers/copyClass/pullUp/pushDown/inlineMethod/changeMethodSignature/extractSuperclass/extractInterface (RefactoringFactory + JavaRefactoringFactory + java-impl-refactorings) |
| `Hints` | `tools.java` | `getFileHints`, `applyHint` — inspection/annotator errors & warnings + quick-fix apply (DaemonCodeAnalyzerImpl + IntentionAction) |
| `IntellijJava` | `tools.java` | `compileAndExecuteInProject` against a project classpath (OrderEnumerator); replaces `SwingJava` |
| `Maven` | `tools.maven` | `getMavenProjects`, `getDependencies`, `runGoals`, `addDependency`, `searchMavenIndex` (MavenProjectsManager + MavenRunner + MavenArtifactSearcher) |
| `RunConfigurations` | `tools.run` | `listRunConfigurations`, `runConfiguration` (RunManager + ProgramRunnerUtil) — beyond-parity |
| `Vcs` | `tools.vcs` | `getVcsStatus` — provider-agnostic changed/unversioned files (ChangeListManager) — beyond-parity |
| `Terminals` | `tools.terminal` | `openLocalTerminal`, `typeCommand`, `closeTerminal` (TerminalToolWindowManager + ShellTerminalWidget) |

The Project view also shows an **Anahata** node (`AnahataTreeStructureProvider`) linking to `anahata.md`, and the diff renderer paints per-line **gutter comment bubbles** from `FullTextResourceUpdate.getLineComments()`.

**Editable diff visualization**: `ui.IntellijTextResourceWriteRenderer` (a `ParameterRenderer`) renders every core `AbstractTextResourceWrite` argument (full-file update, replacements, line edits) as an editable side-by-side `DiffRequestPanel` — current-on-disk vs proposed — with edits written back to the tool call via `setModifiedArgument` (Kryo-cloned `manualOverride`). Registered per DTO type via `ParameterRendererFactory` in the `IntellijAsiContainer` static block.

Shared PSI/VFS resolution lives in `internal.JavaPsi` (used by CodeRefiner/BatchCodeRefiner/Refactor). Structure/alerts context providers use the IntelliJ project model (`ProjectRootManager`/`ProjectFileIndex`) and the public `WolfTheProblemSolver` API — no reflection.

**Settings, status bar & notifications**: a native Settings page (`AnahataConfigurable` under Settings → Tools → Anahata ASI) edits the persisted `AnahataSettings` (default provider/model, read by `IntellijAgiConfig` on session creation); an `AnahataStatusBarWidget` shows the active-session count and focuses the tool window on click; balloon events go through the `Anahata ASI` notification group via `AnahataNotifications`.

**Tool window chrome**: the tool window uses the Anahata brand logo (`/icons/anahataToolWindow.png` + `@2x`, sized from the official wordmark) as its stripe icon, shows the full wordmark as a header banner inside the panel (`/icons/anahataHeader.png`), and exposes native title-bar actions (New Session, Import Session, Show Dashboard, Preferences) wired to the dashboard's `createNew`/`importSession`/`showPreferences`. The dashboard's own Swing toolbar is hidden (`setToolBarVisible(false)`) so those controls live in the native tool-window header instead.

**Context-curation UI** (declared in `plugin.xml`): `AgiContextDecorator` (`projectViewNodeDecorator`) badges files that are in a session's context; `AgiContextActionGroup`/`ToggleAgiContextAction` add a per-session "AGI Context" toggle submenu to the Project-view popup. Both reach live sessions through `IntellijAsiContainer.getInstances()` and `internal.AgiContext` (the bridge to each session's `ResourceManager`).

**Native theming** is deliberately NOT applied to the chat panels: they live in `anahata-asi-swing`, which must not depend on IntelliJ (`JBColor`/`JBUI`) or it breaks the NetBeans/Desktop builds. Standard Swing components already inherit the IDE LAF; the ~456 hardcoded `Color` usages (charts, token colors, status) would need a pure-Swing `UIManager`-based refactor of the shared module, validated across all three hosts — tracked as a separate effort.

## 5. Not Yet Ported (see `ROADMAP.md`)
- **Structured test results** — attach to the test process's event listener (list/run done).
- **Git-specific VCS ops** — branch/commit/log/blame need `git4idea`, which is not a resolvable Maven artifact (the generic status is done via `ChangeListManager`).
- **Route `CodeRefinementBatch` through the diff panel** — so `refine` also renders as an editable diff (currently returns a textual unified diff).

## 6. Build note
The parent pins Lombok `1.18.46` (JDK 24-safe). The earlier `1.18.34` crashed a full `@Data`/`@Getter` recompile on JDK 24 with `TypeTag.UNKNOWN`; that is resolved.

Força Barça!
