/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI IntelliJ Plugin — Parity Roadmap

Feature parity target: the NetBeans plugin (`anahata-asi-nb`). Both plugins are thin IDE
adapters over the shared `anahata-asi-core` engine (session lifecycle, context metabolism,
providers, HITL tool loop, Kryo passivation, and the core toolkits: `Resources`, `Shell`,
`Java`, `Session`, `History`, `Host`, `Audio`). A plugin only adds **IDE-native toolkits,
context providers, and UI**.

## Status legend
✅ done & compiling · 🟡 partial · ⬜ not started · 🔗 needs a new pom dependency

## Parity matrix

| NetBeans toolkit | IntelliJ status | IntelliJ API target | Notes |
|---|---|---|---|
| (core) file/shell/session | ✅ (via core) | — | works out of the box |
| `Projects` | ✅ | `ProjectManager`, `CompilerManager`, `ProjectRootManager` | open/close, structure+alerts context, `anahata.md`, `buildProject`, `saveAllDocuments`. Deeper module/source-group structure = 🟡 |
| `CodeModel` | ✅ | PSI (`JavaPsiFacade`, `PsiShortNamesCache`, `ClassInheritorsSearch`) | read-action wrapped |
| `Editor` | ✅ | `FileEditorManager`, `Editor`, `Caret` | open/list/close + live caret/selection/visible context |
| `IDE` | ✅ | `PathManager`, `ProjectView`, `ToolWindowManager` | `monitorLogs`, `selectIn`, tool-windows report+context |
| `CodeRefiner` (V3) | ✅ | `JavaCodeStyleManager`, `CodeStyleManager` | imports/reformat/annotations, write-command |
| `Refactor` | ✅ | `RefactoringFactory`, `JavaRefactoringFactory`, `java-impl-refactorings` `*Processor` | **complete** (14 tools): rename/renameMember/safeDelete/whereUsed/moveClass/moveMembers/copyClass/pullUp/pushDown/inlineMethod/changeMethodSignature/extractSuperclass/extractInterface. Dep `com.jetbrains.intellij.java:java-impl-refactorings` (transitives excluded) |
| `BatchCodeRefiner` (V4) | ✅ | PSI write-command, `PsiElementFactory`, `JavaCodeStyleManager` | flagship AST member splice (`refine`): insert/update/delete/move, returns unified diff |
| `Hints` | ✅ | `DaemonCodeAnalyzerImpl.runMainPasses`, `HighlightInfo`, `IntentionAction` | `getFileHints` + `applyHint` (apply quick-fix) |
| `Maven` | ✅ | `MavenProjectsManager`, `MavenRunner`, `MavenArtifactSearcher` | `getMavenProjects`/`getDependencies`/`runGoals`/`addDependency`/`searchMavenIndex`. Deps `maven`/`maven-model`/`maven-server`/`repository-search-common` (transitives excluded, provided) |
| `Terminals` (NbTerminal) | ✅ | `TerminalToolWindowManager`, `ShellTerminalWidget` | `openLocalTerminal`/`typeCommand`/`closeTerminal`. Dep `com.jetbrains.intellij.terminal:terminal` (transitives excluded) |
| `IntellijJava` (NbJava) | ✅ | `OrderEnumerator` classpath + core `Java` | `compileAndExecuteInProject` — in-project compile/execute ("Singularity Loop"); replaces `SwingJava` |
| `RunConfigurations` (beyond-parity) | ✅ | `RunManager`, `ProgramRunnerUtil` | `listRunConfigurations`/`runConfiguration`; structured test results ⬜ |
| `Vcs` (beyond-parity) | 🟡 | `ChangeListManager` (generic VCS) | `getVcsStatus`; Git-specific branch/commit/log need `git4idea` (not a resolvable artifact) |

## UI / IDE-surface parity (separate from toolkits)

| NetBeans feature | IntelliJ status | Target |
|---|---|---|
| Editable side-by-side diff of tool-call args vs file (validate-before-apply, sync edits back) | ✅ | `IntellijTextResourceWriteRenderer` (`DiffManager`/`DiffRequestPanel`), registered via `ParameterRendererFactory` in the `IntellijAsiContainer` static block for the core write DTOs; write-back via `setModifiedArgument` (Kryo-cloned `manualOverride`) |
| IDE-fidelity resource viewer | ⬜ | `EditorTextField` / `EditorFactory` + `LightVirtualFile` |
| Virtual "Anahata" folder in project tree | ✅ | `AnahataTreeStructureProvider` (declared in `plugin.xml`) — links to `anahata.md` |
| Diff gutter comment bubbles | ✅ | document markup + `GutterIconRenderer` on the proposed pane (from `getLineComments()`) |
| File/project context badges + "AGI Context" right-click menu | ✅ | `ProjectViewNodeDecorator` (`AgiContextDecorator`) + dynamic `ActionGroup` (`AgiContextActionGroup`/`ToggleAgiContextAction`) in `ProjectViewPopupMenu`; reaches live sessions via `IntellijAsiContainer.getInstances()` + `internal.AgiContext` |

## Cross-cutting conventions (already established)
1. **PSI threading** — reads in `ReadAction.compute`; mutations in `invokeAndWait` + `WriteCommandAction`. See `anahata.md` §3.
2. **No-arg constructors + lazy handle resolution** for all toolkits.
3. **Drop the NB reflection/internal-class hacks** — IntelliJ has clean public APIs for logs, terminals, node decoration, and output consoles.
4. **`plugin.xml` grows** as editor/IDE actions, node decorators, and popup items are added (currently only the tool window is declared).
5. **Canonical member FQN** scheme `pkg.Type.name(erasedArg,...)` for methods, `pkg.Type.field` for fields — shared across CodeModel/CodeRefiner/Refactor (candidate for extraction into a shared `JavaPsi` helper — see DRY note below).

## Done since initial roadmap
- ✅ Shared `internal.JavaPsi` helper extracted; `Editor`/`CodeRefiner`/`Refactor` de-duplicated.
- ✅ `ProjectStructureContextProvider` upgraded to a source-root-aware PSI/`ProjectFileIndex` map (main vs test roots).
- ✅ `ProjectAlertsContextProvider` moved off reflection to the public `WolfTheProblemSolver` API (`isProblemFile` + `FileTypeIndex`), read-action wrapped.
- ✅ `BatchCodeRefiner` V4 (`refine`) — PSI member splice returning a unified diff.
- ✅ Context-curation UI — Project-view "AGI Context" badges + per-session toggle menu, wired in `plugin.xml`; live sessions reached via `IntellijAsiContainer.getInstances()` + `internal.AgiContext`.

- ✅ Editable diff visualization — `IntellijTextResourceWriteRenderer` renders every core write-tool argument as an editable side-by-side diff.
- ✅ `Maven` — `getMavenProjects`/`getDependencies`/`runGoals`/`addDependency`.
- ✅ Advanced `Refactor` — `moveClass` + `moveMembers` (`JavaRefactoringFactory`).
- ✅ `IntellijJava` (NbJava) — `compileAndExecuteInProject` against a project classpath; replaces `SwingJava`.
- ✅ `Hints` — `getFileHints` lists inspection/annotator errors & warnings for a file.
- ✅ Container leak fix — containers deregister from the registry on project dispose.

- ✅ Maven `searchMavenIndex`; Hints `applyHint`; diff gutter comment bubbles; `AnahataTreeStructureProvider`.
- ✅ Beyond-parity: `RunConfigurations` (list/run) and `Vcs` (generic status).
- ✅ `Terminals` (open/type/close) — terminal-plugin dep wired.

- ✅ Advanced `Refactor` — pullUp/pushDown/inlineMethod/changeMethodSignature/extractSuperclass/extractInterface + `copyClass` (`java-impl-refactorings` + PSI copy). **Refactor is complete.**

## Suggested next tranche (in order)
1. **Structured test results** — attach an `SMTRunnerEventsListener`/`AbstractTestProxy` reader so `runConfiguration` returns pass/fail counts.
2. **Git-specific VCS** — only if `git4idea` becomes resolvable (bundled-IDE dep); else stay on the generic layer.
3. **Route `CodeRefinementBatch` through the write-DTO diff pipeline** so `refine` renders as an editable diff too.
4. **Runtime validation** — the UI-heavy pieces (diff viz, gutter bubbles, context menu, Anahata node) and the process-driving toolkits (Terminals, Run, Maven goals, refactorings) want a sandbox-IDE run to confirm behavior.

> **Dependency note:** the IntelliJ Maven artifacts pull unpublished internal transitives (`ai.grazie.*`, packagesearch) that resolve nowhere; the pom therefore excludes all transitives on the three `maven*` deps. They are `provided`, so the real classes come from the running IDE.

> **Offline note:** `org.jetbrains.idea.maven`, `org.jetbrains.plugins.terminal`, and `git4idea` are not in the local `.m2`; items 3–5's gated toolkits need online dependency resolution (or depend on the bundled `ideaIC` distribution) before they can be built/verified.
