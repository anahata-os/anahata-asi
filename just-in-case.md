# Anahata ASI Session Summary & Architecture Handover (2026-09-30)

## 1. Executive Summary
This session solved the multi-second latency bottleneck during prompt assembly in NetBeans (`anahata-asi-nb`), specifically the project structure scan which was taking **12.8+ seconds** per turn when supertypes and javadocs were enabled.

Through root-cause profiling, architecture redesign, and clean implementation, project structure scans were reduced from **12,838 ms down to 30 ms (a 428x speedup)** for the ASM path, and down to **~300-900 ms** for full Javac AST extraction with Javadocs across 180 classes. An AST metadata cache was also created to make warm-turn scans virtually instantaneous.

---

## 2. Problems Identified & Resolved

### A. The 12.8s Per-File Javac Pipeline
- **Root Cause**: `JavaSourceGroup.resolveAstMetadata()` was executing `JavaSource.forFileObject(fo)` inside a loop across 60+ individual `.java` files, requesting `controller.toPhase(Phase.ELEMENTS_RESOLVED)`.
- **Bottleneck**: In NetBeans, javac task scheduling serializes on compiler locks (`TaskProcessor`/`ParserManager`). Spinning up 60 independent compiler pipelines caused massive lock contention and burned ~12.8s per turn.
- **Fix**: Replaced the per-file loop with two clean strategies:
  1. **Fast Strategy (`ScanStrategy.ASM_SIG`)**: Directly parses NetBeans' pre-compiled binary `.sig` signature files in `~/.cache/netbeans/31/index/s1/java/16/classes/.../*.sig` using OW2 ASM 9.10 (`ClassReader`). Extracts ElementKind, supertypes, and inner classes in **~6 ms** for 180 classes.
  2. **Deep Strategy (`ScanStrategy.JAVASOURCE_AST`)**: Used when `showJavadoc == true`. Executes **one single** `JavaSource.create(cpInfo, files)` task advancing to `Phase.ELEMENTS_RESOLVED` once for the entire source group, extracting supertypes, Javadocs, and inner classes together in ~900 ms.

### B. Inner Class Duplication in ASI Core (150,000 Token Bloat)
- **Root Cause**: `JavaSource.create(cpInfo, files)` executes `js.runUserActionTask(controller -> ...)` once per file in `files`. The inner loop was iterating over *all* components in the source group instead of just the ones belonging to `controller.getFileObject()`, duplicating every inner class 168 times.
- **Fix**: Restricted population to `controller.getFileObject()` and added `comp.getChildren().clear()` for safe idempotency.
- **Result**: Core tokens dropped from **160,865 down to 11,064 tokens** (saving ~150k tokens per turn).

### C. Direct Filesystem Traversal
- **Root Cause**: The old scanner was performing 3 redundant steps: querying Lucene `ClassIndex.getDeclaredTypes`, mapping handles to files via `SourceUtils.getFile` (~160ms), and walking the directory tree anyway.
- **Fix**: Simplified to walk the directory tree directly (like IntelliJ does). Package names and `FileObject`s are resolved immediately, and metadata is enriched via ASM `.sig` or `JavaSource`.

### D. In-Memory AST Metadata Cache
- **Design**: Created standalone records `CachedAstMetadata.java` and `CachedInnerClass.java` under `uno.anahata.asi.nb.tools.project.components`.
- **Mechanism**: Binds resolved AST metadata (element kind, supertypes, Javadoc summary, inner classes) to `(FileObject.getPath(), FileObject.lastModified())`.
- **Benefit**: On warm turns, unchanged files are resolved from the in-memory cache in 0 ms. Only newly added or edited files are passed to `JavaSource.create(cpInfo, unindexedFiles)`.
- **Quality**: Full ASI-grade Javadoc for all records, methods, and constructors; zero defensive method-start null checks.

### E. Code Cleanups
- Removed duplicate/orphaned Javadoc comments on constructors in `JavaSourceGroup.java` and `ProjectStructure.java`.
- Stripped enclosing class prefixes from inner class display names in `ProjectComponent.java`.
- Cleaned unused imports and removed inner duplicate records from `JavaSourceGroup.java`.

---

## 3. Context Provider Parallelism & EDT Health Check

### A. ContextManager Parallelism Review
- Currently, `ContextManager.buildRagMessage()` executes all context providers **sequentially in a single thread**:
  `Total RAG Time = Sum(all providers) = ~5.5s to 8.8s`.
- On this 24-core host, providers can be parallelized:
  - **Tool Calls MUST remain sequential** (to preserve causality, error recovery, and filesystem consistency).
  - **Context Providers CAN and SHOULD be parallelized** (they are read-only observers).
  - **Requirement**: Must use **Strict Order-Preserving Assembly** so the prompt sections are concatenated deterministically, preserving prompt prefix caching (KV cache) and attention quality.
  - Expected wall-clock turn time drops from ~5.5s down to ~0.5s–1.5s.

### B. EDT Health Check on Context Providers
- Audited all `populateMessage` implementations across all modules for `SwingUtilities.isEventDispatchThread()` checks.
- Found **only 1 occurrence** in the entire codebase: `ProjectAlertsContextProvider.java:44`.
- Confirmed that `populateMessage` should never rely on running on the EDT; background tasks (like `SwingTask`) should handle long-running operations off the EDT, and only small UI-bound queries should touch the EDT via `SwingUtils.runInEDTAndWait`.

---

## 4. Current File Status & Safe Reloading (2026-09-30 Handover)
- Modified files in `anahata-asi-nb`:
  * `ProjectComponent.java`: simple name formatting.
  * `JavaSourceGroup.java`: dual-strategy ASM/.sig + JavaSource AST engine, delegation to `CachedAstMetadata`.
  * `ProjectStructure.java`: `ScanStrategy` tracking, clean Javadocs.
  * `CachedAstMetadata.java`: standalone AST cache record (added).
  * `CachedInnerClass.java`: standalone inner class record (added).
- All files compile cleanly with zero errors on both Maven and NetBeans IDE.
- Sessions are backed up to disk via Kryo on every turn and survive `nbmreload` with 100% state preservation.

---

## 5. Session Handover & Achievements (2026-10-01: IntelliJ Maven & Diagnostics Overhaul)

### A. Hints Toolkit Fix (`Hints.getFileHints`)
- **Root Cause 1 (`assertUnderDaemonProgress`)**: In modern IntelliJ platforms (2025/2026+), `DaemonCodeAnalyzerImpl.runMainPasses` asserts that the executing progress indicator is an instance of `DaemonProgressIndicator`. `Hints.java` was using `EmptyProgressIndicator`, causing an immediate `IllegalStateException`.
- **Root Cause 2 (Missing `HighlightingSession`)**: Highlight passes require an active session bound to the indicator. Wrapped pass execution inside `HighlightingSessionImpl.runInsideHighlightingSession(psiFile, scheme, range, false, session -> ...)`.
- **Syntax Filter**: Filtered out raw syntax/coloring tokens where `info.getDescription() == null`, preventing raw language tokens from polluting inspection hints.
- **Verification**: Tested live in-JVM on `Hints.java` — returned 7 clean warnings in ~1.9s with 0 errors.

### B. IntelliJ Maven Runtime & Repository Context (`populateMessage`)
- Implemented `IntellijMaven.populateMessage(RagMessage)` to provide live JIT Maven context on every turn:
  * Active Maven Home (`settings.getMavenHomeType()`, bundled Maven 3.9.16).
  * Local repository location (`~/.m2/repository`).
  * User `settings.xml` path and offline mode status.
  * Index Manager initialization status (`MavenIndicesManager.isInit()`).
  * Configured repositories matrix (Local index + all remote repositories across open projects with live status badges).
  * Complete list of all 14 imported Maven projects with Maven IDs, packaging, and directory paths.

### C. Native Artifact-Centric Maven Search (`IntellijMaven.searchMaven`)
- Created clean IntelliJ DTOs under `uno.anahata.asi.intellij.tools.maven`:
  * `MavenArtifactGroup`: Consolidated artifact model with `groupId`, `artifactId`, `latestVersion`, `totalVersionsCount`, and sorted `versions` list.
  * `MavenSearchReport`: Search container with `query`, pagination (`startIndex`, `totalCount`), and `List<MavenArtifactGroup>`.
- **Three Search Modes**:
  1. **Instant Coordinate Resolution (0 ms)**: When `groupId` and `artifactId` are provided, resolves all versions directly from `MavenGAVIndex` without executing search queries.
  2. **Group Navigation**: When only `groupId` is provided, lists all artifacts belonging to that group in 0 ms.
  3. **Keyword Token Search**: Uses `MavenArtifactSearcher` to match coordinates, grouping matching versions under single artifact entries with `ComparableVersion` (newest-first) sorting and pre-release stability filtering.
- **Deleted**: Removed deprecated `searchMavenIndex(String query, Integer maxResults)`.

### D. Canonical `MavenBuildResult` in Core (`uno.anahata.asi.toolkit.maven`)
- Moved `MavenBuildResult` (and its inner `ProcessStatus` and `BuildPhase`) to `uno.anahata.asi.toolkit.maven` in `anahata-asi-core`.
- Both `anahata-asi-nb` and `anahata-asi-intellij` now share the exact same DTO without code duplication.

### E. Critical Upgrade to `IntellijMaven.runGoals`
- **Full Parameter Support**:
  * `projectPath`, `goals`, `profiles`, `properties` (`Map<String, String>`), `options` (`List<String>`), `skipTests` (`Boolean`), `vmOptions` (`String`), `timeoutSeconds` (`Integer`).
- **Telemetry-Driven Phase Capture (Zero Reflection)**:
  * IntelliJ injects its EventSpy (`IntellijMavenSpy`) into every build, streaming structured IPC events over stdout prefixed with `[IJ]-3-`.
  * Intercepts `MojoStarted`, `MojoSucceeded`, and `MojoFailed` to record each phase's `name` (goal), `plugin` (id), `success` status, and `durationMs` into `List<BuildPhase> phases`.
  * Strips all `[IJ]-` lines from `stdOutput` so the output is 100% clean Maven text.
- **Zero Log Noise / UI Freeze Prevention**:
  * **Completely eliminated per-line streaming to `ctx.log()`**. Writes untruncated log to disk (`/tmp/anahata-intellij-maven-xxx.log`).
  * Emits only 2 milestone logs (launch and completion).
  * Reduced tool execution log token footprint from **390,000+ tokens down to ~50 tokens**.
  * Returns `MavenBuildResult` with status, exitCode, last 100 lines of stdOutput/stdError, logFile path, and populated `phases`.

### F. NetBeans `Maven.java` Enhancements
- Added `resolvePropertyVersion` to resolve expressions like `${netbeans.version}` and `${project.version}` against the live Maven POM model.
- Added relative positioning (`beforeDependency`, `afterDependency`) and clean XML comment insertion above dependencies via DOM manipulation.
- Updated to import `MavenBuildResult` from `core`.

### G. Plugin Reloading Architecture
- **Rule**: `IntellijProjects.buildProject` (make) only compiles classes to `target/classes`. It does **NOT** package the plugin distribution archive.
- To reload the IntelliJ plugin, `anahata-asi-intellij` must be built via `mvn package` (or `mvn clean package -DskipTests`), after which clicking "Reload plugin" loads the updated artifact.

---

## 6. Session Handover & Achievements (2026-10-02: CodeModel Library Sources & Maven DOM Architecture)

### A. IntelliJ Maven Build Execution (`IntellijMaven.runGoals`)
- **Parity with NetBeans**: Verified full parity against NetBeans `Maven.runGoals`.
- **IntelliJ EventSpy Telemetry**: Automatically parses IntelliJ's `[IJ]-3-` telemetry events for `MojoStarted`, `MojoSucceeded`, and `MojoFailed` into structured `List<BuildPhase> phases` without reflection.
- **Output Management & Token Protection**:
  * Output $\le$ 100 lines: 100% full output captured.
  * Output > 100 lines: Sliced with **Head (25 lines) + Tail (75 lines)** capping to protect prompt context, with truncation marker pointing to disk log.
  * Full untruncated raw output continuously streamed to disk at `/tmp/anahata-intellij-maven-*.log`.
- **Live Verification**: Successfully executed `clean package` on `anahata-asi-intellij` inside the live JVM with `exitCode=0` and 8 structured phases captured.

### B. CodeModel Library & Decompiled Source Loading (`CodeModel.loadTypeSources`)
- **Root Cause of Failed Source Loads**:
  1. `getUrlOfClass` returned `vFile.getPath()`, which for JAR classes contains `!/` (e.g. `/path/to/maven.jar!/.../MavenDomUtil.class`). Passing this to `Path.of(...).toURI()` caused `Files.exists` to fail because `!` is not a directory on standard OS filesystems.
  2. `cl.getContainingFile()` for compiled library classes returns a `.class` stub. The actual attached source or decompiled code lives at `cl.getNavigationElement().getContainingFile()`.
- **Dual-Path Solution in `loadTypeSources`**:
  1. **Physical Files (`vFile.isInLocalFileSystem()`)**: Registered directly via local `Path` using `ResourceManager.registerPaths` for full editing, diff gutter bubbles, and VCS tracking.
  2. **Library / Attached / Decompiled Sources (`StringHandle`)**:
     * Follows `cl.getNavigationElement().getContainingFile()` to extract full Kotlin/Java source from attached source JARs or Fernflower decompiler in memory (`psiFile.getText()`).
     * Wraps in `StringHandle(fqn + " (" + fileName + ")", text)` and sets `handle.setContextPath(vf.getPath())` to preserve origin JAR traceability.
     * Registered as a managed in-memory resource via `ResourceManager.registerHandle(handle, actor)`.
  3. **Batch Loading (`loadTypeSourcesByFqn`)**: Upgraded to accept `List<String> fqns` for 100% NetBeans parity, allowing multiple types to be registered in a single turn.
- **`UrlHandle` Incompatibility**:
  * Confirmed that `UrlHandle` is designed strictly for HTTP/HTTPS remote URLs (`HttpURLConnection`) and throws `ClassCastException` on `JarURLConnection`, cannot parse IntelliJ's `jar:///` VFS scheme, and cannot handle on-the-fly decompiled bytecode. `StringHandle` is the correct, proven design.

---

## 7. Active Resources Inventory (Handover Snapshot)

### Managed Context Resources:
1. `IntellijMaven.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-intellij/src/main/java/uno/anahata/asi/intellij/tools/maven/IntellijMaven.java`
2. `just-in-case.md`: `/home/pablo/NetBeansProjects/anahata-asi-parent/just-in-case.md`
3. `MavenBuildResult.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/toolkit/maven/MavenBuildResult.java`
4. `Maven.java` (NetBeans): `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/src/main/java/uno/anahata/asi/nb/tools/maven/Maven.java`
5. `CodeModel.java` (IntelliJ): `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-intellij/src/main/java/uno/anahata/asi/intellij/tools/java/CodeModel.java`
6. `CodeModel.java` (NetBeans): `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/src/main/java/uno/anahata/asi/nb/tools/java/CodeModel.java`
7. `JavaTypeSource.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/src/main/java/uno/anahata/asi/nb/tools/java/JavaTypeSource.java`
8. `IntellijHandle.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-intellij/src/main/java/uno/anahata/asi/intellij/resources/handle/IntellijHandle.java`
9. `StringHandle.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/agi/resource/handle/StringHandle.java`
10. `UrlHandle.java`: `/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/agi/resource/handle/UrlHandle.java`
11. `MavenDomUtil.kt` (In-Memory via StringHandle): `org.jetbrains.idea.maven.dom.MavenDomUtil`

---

## 8. Pending Tasks (Post-Reload Roadmap)

### Task 1: Native IntelliJ Maven DOM Migration in `IntellijMaven.java`
- **Replace StAX XML Stream Parsing in `getDeclaredDependencies`**:
  * Use IntelliJ's native DOM API:
    ```java
    MavenDomProjectModel domModel = MavenDomUtil.getMavenDomProjectModel(project, pomVf);
    List<MavenDomDependency> deps = domModel.getDependencies().getDependencies();
    ```
  * Map typed fields (`getGroupId()`, `getArtifactId()`, `getVersion()`, `getScope()`, `getClassifier()`, `getType()`, `getExclusions()`) into `List<DependencyScope>`.
  * Completely delete the 120 lines of StAX parsing (`parseDeclaredDependencies` and XML reader loops).
- **Replace String Searching in `addDependency`**:
  * Use IntelliJ's native DOM API:
    ```java
    WriteCommandAction.runWriteCommandAction(project, () -> {
        MavenDomProjectModel domModel = MavenDomUtil.getMavenDomProjectModel(project, pomVf);
        MavenDomDependency dep = domModel.getDependencies().addDependency();
        dep.getGroupId().setStringValue(groupId);
        dep.getArtifactId().setStringValue(artifactId);
        if (version != null && !version.isBlank()) dep.getVersion().setStringValue(version);
        if (scope != null && !scope.isBlank()) dep.getScope().setStringValue(scope);
    });
    ```
  * Preserves XML code-style formatting, comments, and triggers project model reimport cleanly.

### Task 2: OS Clipboard Image Pasting Fix
- **Problem**: In IntelliJ, pressing `Ctrl+V` in the chat input text area is intercepted by IntelliJ's global `ActionManager` (`$Paste` / `EditorPaste`), which routes through `CopyPasteManager` and drops binary image flavors (`DataFlavor.imageFlavor`).
- **Solution**: In `InputPanel` (or `IntellijAgiConfig`), register an explicit `Ctrl+V` key listener/action on `inputTextArea` that checks `Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null)` directly for `DataFlavor.imageFlavor`. If image data is present, attach the image to the chat; otherwise delegate to default text paste.

### Task 3: Test `CodeModel.loadTypeSourcesByFqn` on the 6 Maven DOM Types
- After plugin reload, verify batch loading with:
  `CodeModel.loadTypeSourcesByFqn([`
  `  "org.jetbrains.idea.maven.dom.MavenDomUtil",`
  `  "org.jetbrains.idea.maven.dom.model.MavenDomProjectModel",`
  `  "org.jetbrains.idea.maven.dom.model.MavenDomDependencies",`
  `  "org.jetbrains.idea.maven.dom.model.MavenDomDependency",`
  `  "org.jetbrains.idea.maven.project.MavenProject",`
  `  "org.jetbrains.idea.maven.utils.MavenArtifactUtil"`
  `])`

---

## 9. Session Handover & Achievements (2026-10-03: Maven Parity, Clipboard Image Pasting, UI Diff Header & Line Comments Push-Down, BatchCodeRefiner Deduplication)

### A. IntelliJ Maven DOM Migration (`IntellijMaven.java`)
- **Native Maven DOM Migration in `getDeclaredDependencies`**:
  * Replaced 120 lines of raw StAX XML stream parsing with IntelliJ's native DOM API:
    ```java
    MavenDomProjectModel domModel = MavenDomUtil.getMavenDomProjectModel(project, pomVf);
    List<MavenDomDependency> deps = domModel.getDependencies().getDependencies();
    ```
  * Grouped directly via `groupDeclaredDependencies(List<MavenDomDependency>)`, completely eliminating the temporary `RawDependency` intermediate class.
- **Native Maven DOM Migration in `addDependency`**:
  * Replaced raw string concatenation with `MavenDomDependencies.addDependency()` executed inside `WriteCommandAction`.
  * **Managed Version Resolution**: Queries `mp.findManagedDependencyVersion(groupId, artifactId)`. If the version matches `dependencyManagement`, the redundant `<version>` tag is automatically omitted from `pom.xml` to prevent IDE warnings.
  * **XML Comment Support**: Inserts descriptive XML comments (`<!-- comment -->`) directly above the `<dependency>` tag in the DOM.
  * **Formatting**: Automatically reformats the added DOM block using IntelliJ's `CodeStyleManager`.
  * **Transitive Resolution & Return DTO**: Synchronously executes `runGoals('dependency:resolve')` and returns `AddDependencyResult` (moved to `uno.anahata.asi.toolkit.maven` in `core` for full parity with NetBeans).
  * **Deprecation Fix**: Replaced deprecated `MavenArtifactUtil.getArtifactFile` with `resolveLocalArtifactPath` helper.
  * **Live Verification**: Verified on `anahata-asi-web` with `org.slf4j:slf4j-api` — created `<dependencies>` section, omitted managed `<version>`, added comment, reformatted XML, and resolved dependencies in 701 ms with `BUILD SUCCESS`.

### B. OS Clipboard Image Pasting Fix in IntelliJ
- **Root Cause Identified**:
  * In NetBeans and Desktop, `Ctrl+V` routes directly through Swing `InputMap` to `inputTextArea.paste()` -> `AgiTransferHandler`.
  * In IntelliJ, `IdeEventQueue` intercepts `Ctrl+V` globally for `$Paste` (`EditorPaste`). Because standard Swing components lack IntelliJ's `PasteProvider`, IntelliJ deemed paste disabled, consumed the keystroke, and dropped binary `DataFlavor.imageFlavor` data.
- **Clean Architectural Solution (No Subclassing of Agi or AgiPanel)**:
  * In `anahata-asi-swing`: Added `public void onAgiPanelInitialized(AgiPanel agiPanel)` lifecycle hook to `SwingAgiConfig`, invoked at the very end of `AgiPanel.initComponents()`.
  * In `anahata-asi-intellij`: `IntellijAgiConfig` overrides `onAgiPanelInitialized` to register a component-scoped `AnAction` for `Ctrl+V` / `Cmd+V` on `inputTextArea` using `CustomShortcutSet`.
  * **Verification**: Component-scoped action takes precedence over global keymap actions. Pressing `Ctrl+V` with an image on the clipboard immediately invokes `inputTextArea.paste()`, creating a temporary PNG and attaching it to the chat preview.

### C. Session Nickname & ToolWindow Tab Synchronization
- Added `IntellijAsiContainer.updateToolWindowTabTitle(Agi agi)`.
- Invoked inside `IntellijAsiContainer.onSessionContextChanged` on the EDT whenever `"nickname"` property changes, dynamically updating the Anahata ToolWindow tab header to match the session nickname.

### D. IntelliJ Projects Toolkit & Compiler Diagnostics (`IntellijProjects.java`)
- **Rich Diagnostic Return Value**:
  * Upgraded `buildProject` callback to extract `compileContext.getMessages(CompilerMessageCategory.ERROR)` and `WARNING`.
  * Resolves file path, line number, and column offset via `msg.getNavigatable() instanceof OpenFileDescriptor`.
  * Returns formatted multi-line summary with categorized error/warning lists instead of just counts.
- **ToolContext Error Logging**:
  * Captured `final ToolContext ctx = getToolContext();` before launching async build.
  * Dispatches every compiler error to `ctx.error(...)` and warning to `ctx.log(...)` so diagnostics appear in the tool response tabs.
- **Coding Standards Cleanups**:
  * Eliminated all 8 FQN violations in method bodies (imported `Optional`, `SwingUtilities`, `Method`, `CompileStatusNotification`, `Collection`).
  * Removed redundant `compileContext != null` and `ctx != null` checks.

### E. IDE Navigation & Selection (`IDE.java` and `SelectInTarget.java`)
- Created outer enum `SelectInTarget` (`PROJECTS`, `STRUCTURE`, `FILES`).
- Upgraded `IDE.selectIn(String path, SelectInTarget target)`:
  * EDT-safe execution: checks `Application.isDispatchThread()` to run directly if already on EDT (fixing the `invokeAndWait` deadlock when clicking from UI buttons), otherwise uses `invokeAndWait`.
  * `PROJECTS`: Activates Project tool window and executes `ProjectView.select(null, vf, true)`.
  * `STRUCTURE`: Opens file and activates Structure tool window.
  * `FILES`: Reveals file in OS file manager via `RevealFileAction`.
- Added `IDE.selectInProjects(String path)` alias for full NetBeans parity.

### F. Diff Viewer UI Header & Actions (`IntellijTextResourceWriteRenderer.java`)
- **Rich Header Panel**:
  * Built `createHeaderPanel` placed at `BorderLayout.NORTH` above `diffPanel.getComponent()`.
  * Status label: `Proposed Changes:` / `Applied Changes:` / `Changes (Declined):`.
  * Authentic File Icon: Resolved via `agiPanel.getAgiConfig().getIconProvider().getIconFor(resource)`.
  * Action Buttons: Calls `ResourceUiRegistry.populateActions(...)` to inject **`Open in Editor`** and **`Select in Project`** buttons directly into the diff header.
  * AI Comments List: Renders formatted line comments summary aligned to the top right.
- **Authentic Gutter Commentary Icon**:
  * Replaced `AllIcons.General.Balloon` with authentic 16x16 Anahata logo (`AnahataFileIconProvider.getFileIcon()`) in `CommentGutterRenderer`. Hovering displays the AI comment tooltip.

### G. Line Comments Push-Down to Core (`core`, `intellij`, `nb`)
- Moved `DiffCommentUtils` from `anahata-asi-nb` to `uno.anahata.asi.toolkit.resources.text` in `anahata-asi-core`.
- Pushed down `public List<LineComment> calculateLineComments(Agi agi)` to `AbstractTextResourceWrite`:
  * `FullTextResourceUpdate`: returns explicit `lineComments`.
  * `TextResourceReplacements`: locates target occurrences, maps to proposed line numbers via cumulative line-shift math, and returns `List<LineComment>`. Extracted `ReplacementEvent` as a javadocced private static record.
  * `TextResourceLineEdits`: aggregates insertions, replacements, deletions with cumulative line shifts.
  * `CodeRefinementBatch`: returns `calculatedComments` from AST surgery.
- Updated NetBeans (`TextResourceReplacementsRenderer`, `TextResourceLineEditsRenderer`) and IntelliJ (`IntellijTextResourceWriteRenderer`) to delegate directly to `update.calculateLineComments(agi)`.
- Enforced fail-fast non-null validation on `TextResourceReplacements.replacements` (`@NonNull`).
- Upgraded fallback exception logging to `log.error("Failed to capture original content...", e)` with full stack trace.
- Eliminated all double Javadoc blocks and defensive null checks across all DTOs and handles.

### H. Deduplication of `BatchCodeRefiner` (IntelliJ)
- Centralized all PSI AST mutation logic inside `CodeRefinementBatch.applyIntentToPsi(...)`.
- Removed ~145 lines of copy-pasted duplicate methods (`parseMember`, `insertMember`, `requireAnchor`, `requireMember`) from `BatchCodeRefiner.java`.
- Registered `CodeRefinementBatch.class` in `ParameterRendererFactory` in `IntellijAsiContainer.initEnvironment()`, connecting it to `IntellijTextResourceWriteRenderer`.
- Added `SmallTestClass.java` to `uno.anahata.asi.intellij.tools.java.coderefiner` mirroring NetBeans test coverage.

### I. Active Tasks & Roadmap (`tasks.md`)
- Deleted obsolete `ROADMAP.md` and created clean `tasks.md` in `anahata-asi-intellij`.
- **Next Priority**: Task 4 — Structured Test Results in `RunConfigurations` (`SMTRunnerEventsListener` / `AbstractTestProxy`).

---

## 10. Active Working Tree & Modified Files Summary
- **`anahata-asi-core`**:
  * `AbstractTextResourceWrite.java` (added `calculateLineComments(Agi)`)
  * `DiffCommentUtils.java` (moved from NetBeans to core)
  * `FullTextResourceUpdate.java` (implements `calculateLineComments`)
  * `TextResourceReplacements.java` (implements `calculateLineComments`, extracted `ReplacementEvent` record, `@NonNull replacements`, `log.error` with stack trace)
  * `TextResourceLineEdits.java` (implements `calculateLineComments`, `log.error` with stack trace)
  * `AddDependencyResult.java` (moved from NetBeans to core)
- **`anahata-asi-intellij`**:
  * `IntellijMaven.java` (native Maven DOM, `addDependency`, `AddDependencyResult`, no deprecations)
  * `IntellijProjects.java` (rich compiler diagnostic extraction, ToolContext logging, no FQNs)
  * `IDE.java` (EDT-safe `selectIn`, `SelectInTarget` enum)
  * `SelectInTarget.java` (added enum)
  * `IntellijResourceUI.java` (uses `SelectInTarget.PROJECTS`)
  * `IntellijTextResourceWriteRenderer.java` (header panel, actions, comments list, Anahata gutter icon, delegates to `calculateLineComments`)
  * `BatchCodeRefiner.java` (deduplicated, delegates to `CodeRefinementBatch`)
  * `CodeRefinementBatch.java` (centralized PSI AST mutation helpers, `calculateLineComments`)
  * `SmallTestClass.java` (added for AST test coverage)
  * `IntellijHandle.java` (cleaned defensive null checks)
  * `IntellijAgiConfig.java` (wired `onAgiPanelInitialized` for `Ctrl+V` paste action)
  * `IntellijAsiContainer.java` (registered `CodeRefinementBatch` renderer, tab title sync)
  * `tasks.md` (active roadmap)
- **`anahata-asi-nb`**:
  * `CodeRefinementBatch.java` (implements `calculateLineComments`)
  * `TextResourceReplacementsRenderer.java` (delegates to `update.calculateLineComments()`)
  * `TextResourceLineEditsRenderer.java` (delegates to `update.calculateLineComments()`)
  * `TeeInputOutput.java` (cleaned deprecated call)
- **`anahata-asi-swing`**:
  * `SwingAgiConfig.java` (added `onAgiPanelInitialized(AgiPanel)`)
  * `AgiPanel.java` (invokes `agiConfig.onAgiPanelInitialized(this)`)

---

## 11. Session Handover & Architecture Achievements (2026-10-04: The `anahata-asi-ide` Extraction, Handle & UI Strategy Unification, and 1.4.0-SNAPSHOT Bump)

### A. Strategic Extraction of `anahata-asi-ide`
- **Module Creation**: Created `anahata-asi-ide` inheriting from `anahata-asi-parent` and depending on `anahata-asi-swing` (which transitively brings in `anahata-asi-core`).
- **Core Decoupling**: Completely purged `anahata-asi-core` from IDE-specific concerns:
  * Zero VCS imports/classes in core.
  * Zero Maven imports/classes in core.
  * Zero Project model/scoping imports/classes in core.
- **Dependency Hierarchy**:
  ```text
                 anahata-asi-core (Pure AI Engine, Agi, Context, Tools SPI, Resource)
                         │
                 anahata-asi-swing (AgiPanel, ParameterRenderer, UI Controls, RSyntaxTextArea)
                         │
                 anahata-asi-ide (Universal IDE Abstractions, Contracts, DTOs & IdeHandle)
                 ┌───────┴──────────────┬─────────────────────────┐
                 ▼                      ▼                         ▼
          anahata-asi-nb      anahata-asi-intellij      anahata-asi-eclipse (Future)
  ```

### B. Elimination of "Interfacetitis" on Resource Handles
- **Collapsed Handle Hierarchy**: Converted `interface ResourceHandle` into a single canonical `public abstract class ResourceHandle implements Rebindable`.
- **Deleted `AbstractResourceHandle`**: Moved `protected Resource owner;`, getters/setters, and `rebind()` directly into `ResourceHandle`.
- **Decoupled Annex (`handle.getAnnex()`)**: Replaced hardwired `getDiffToHead()` and `getHistory()` with `public List<String> getAnnex() { return Collections.emptyList(); }`.
- **System Instructions Fix**: Updated `Resource.java` so that both `PROMPT_AUGMENTATION` and `SYSTEM_INSTRUCTIONS` resources (such as `anahata.md`) append the annex, guaranteeing that uncommitted diffs and commit history are visible for instructions files!

### C. Universal `IdeHandle` in `anahata-asi-ide`
- Created `uno.anahata.asi.ide.resources.handle.IdeHandle extends ResourceHandle`:
  * Consolidated common fields (`uri`, `path`).
  * Unified physical filesystem checks (`isVirtual() -> false`, `length()`, `exists()`, `openStream()`, `isWritable()`).
  * Unified `getAnnex()` returning working-copy VCS diff markdown and recent commit/local history markdown table.
- **Deduplication in `NbHandle` & `IntellijHandle`**:
  * Both updated to `extends IdeHandle`.
  * Deleted ~200 lines of duplicated VCS and field code across both plugins.

### D. Migration of VCS Layer
- Moved to `uno.anahata.asi.ide.vcs` in a single atomic refactoring session:
  * `AbstractVCS.java`
  * `VcsDiff.java`
  * `VcsFileStatus.java`
  * `HistoryEntry.java`
  * `FastForwardPolicy.java`
- Both `NbVCS` and `IntellijVCS` now extend `uno.anahata.asi.ide.vcs.AbstractVCS`.

### E. Migration of Maven Layer
- Moved to `uno.anahata.asi.ide.tools.maven`:
  * `DependencyScope.java`
  * `DependencyGroup.java`
  * `DeclaredArtifact.java`
  * `MavenBuildResult.java`
  * `AddDependencyResult.java`
- `NbMaven` and `IntellijMaven` share identical Maven DTO models.

### F. Migration of Projects Layer & UI Components
- **Domain Models** (`uno.anahata.asi.ide.tools.project`):
  * `AbstractProjects.java`
  * `ProjectOverview.java`
  * `ProjectStructureScope.java`
  * `AbstractProjectContextProvider.java` (in `...project.context`)
- **UI Components** (`uno.anahata.asi.ide.ui.project`):
  * `ProjectsPanel.java`: Swing panel configuring default scope.
  * `ProjectStructureScopePanel.java`: 9-switch scope grid.
  * `ProjectContextProviderPanel.java`: Scope mode dropdown and dynamic inheritance label.
  * `ProjectContextProviderNode.java`: Reactive tree node listening to `"projectStructureScope"` on provider and `"defaultScope"` on toolkit (unconditional fail-fast binding).
  * `ProjectsToolkitNode.java`: Specialized tree node representing `AbstractProjects`.

### G. UI Architecture Modernization (`*UI` Strategy Pattern)
- **Eliminated UI Interfacetitis**: Replaced disparate renderer interfaces and lambda factories with lightweight UI strategies mirroring `ResourceUI`:
  * `ToolkitUI<T>`: provides `createNode(agiPanel, toolkit, jot)` and `createPanel(toolkit, agiPanel)`
  * `ContextProviderUI<T>`: provides `createNode(agiPanel, provider)` and `createPanel(provider, contextPanel)`
- **ClassHierarchyMap**: Extracted thread-safe generic `ClassHierarchyMap<B, V>` to eliminate duplicate while-loop hierarchy traversals across registries.
- **AbstractIdeAsiContainer**: Universal container base class registering `ProjectsUI` and `ProjectContextProviderUI` automatically.
- **Decoupled ContextPanel**: Removed `AbstractProjects` import and hardwired project scope listeners from `ContextPanel.java`.

### H. UI Polish, Reentrancy Guards & Diagnostics
- **Dynamic Scope Source Resolver**: `ProjectContextProviderPanel` renders clean "Inherit" vs "Custom" dropdown and a dynamic italic label (`Inheriting from: <ancestor>` or `Inheriting from: Projects toolkit default`).
- **Max Depth Labels**: Added `maxDepthInheritLabel` to both `ToolPanel` and `ToolkitPanel` showing effective depth when inheriting (`-1`).
- **Reentrancy Protection**: Installed `private boolean adjusting = false;` guards on `ToolPanel` and `ToolkitPanel` listeners.
- **Color Method**: Added instance method `getToolPermissionColor` on `SwingAgiConfig`, deprecating the static `getColor`.
- **Fixed Javadoc Plugin Failures**: Fixed 5 outdated package-info Javadoc `@link` references across `intellij` and `nb`.

### I. Version Bump to 1.4.0-SNAPSHOT
- Executed `mvn -o -DnewVersion=1.4.0-SNAPSHOT -DgenerateBackupPoms=false versions:set`.
- All 15 reactor POMs bumped to `1.4.0-SNAPSHOT`.
- All changes reviewed, verified with 0 compiler alerts, and committed/pushed to `origin/main`.

---

## 12. Stage 2 Post-Reload Roadmap

1. **Java AST & Code Model DTO Deduplication** (`uno.anahata.asi.ide.tools.java`):
   - Move keychain DTOs: `JavaType`, `JavaMember`, `JavaMemberPage`, `JavaHierarchyNode`.
   - Move refinement DTOs: `CodeRefinementBatch` (base class), `CodeRefinementIntent`, `RelativePosition`, `JavadocIntent`.
   - Update both `nb` and `intellij` to use the unified DTOs, deduplicating ~1,000 lines.
2. **IntelliJ Hints Parity** (`uno.anahata.asi.intellij.tools.java.Hints`):
   - Refactor IntelliJ `Hints` to `extends AbstractHints` and return `List<HintInfo>` from `getFileHints(String filePath)`.
   - Implement `getHintMetadata()` for IntelliJ inspection profile via `InspectionProjectProfileManager`.
3. **Base Editor & Navigation Toolkits** (`uno.anahata.asi.ide.tools.ide`):
   - Define `AbstractEditor` (`openFile`, `getOpenFiles`, `closeAllFiles`) and `AbstractIDE` (`selectInProjects`, `monitorLogs`).
4. **Base Resource UI Strategy & Panel** (`uno.anahata.asi.ide.ui.resources`):
   - Leverage `IdeResourceUI` and `IdeHandlePanel` across future IDE modules (e.g. Eclipse).

---

## 13. Session Handover & Architecture Achievements (2026-10-04: Universal Hints Layer, Live Diagnostic Annex, and KV-Cached Inspection Profile)

### A. Universal Inspection Layer in `anahata-asi-ide` (`uno.anahata.asi.ide.tools.hints`)
- **Strictly Zero False Abstractions & 100% Preservation of Native IDE Semantics**:
  * Avoided forcing false fix abstractions: NetBeans applies fixes by catalog rule ID across the file (`applyHintFix`), whereas IntelliJ applies line-targeted `IntentionAction`s (`applyHint`). Each host IDE preserves its native fix tool and mental model.
  * Extracted genuine semantic commonalities: live diagnostic reporting and rule catalog metadata.
- **Universal DTOs**:
  * `HintInfo.java`: Canonical diagnostic hit in a file with `filePath`, `line`, `column`, `severity` (String preserving localized IDE names), `description`, and `id`. Includes `toMarkdown()` and `toMarkdown(fileName, hints)` helpers.
  * `HintMetadata.java`: Canonical rule definition in catalog with `id`, `displayName`, `description`, `category`, `severity`, and `enabled` boolean state.
  * `package-info.java`: Javadoc documentation for `uno.anahata.asi.ide.tools.hints`.
- **Abstract Toolkit Base (`AbstractHints.java`)**:
  * Universal base class extending `AnahataToolkit` with:
    `public abstract List<HintInfo> getFileHints(String filePath) throws Exception;`
  * Intentionally omits `@AgiTool` annotations so concrete IDE toolkits retain 100% authentic localized prompts and descriptions.

### B. Core Architecture Guidelines in `anahata-asi-ide/anahata.md`
- Documented foundational principles in `anahata-asi-ide/anahata.md`:
  1. Strictly zero false abstractions and 100% preservation of native IDE semantics.
  2. No `@AgiTool` annotations on abstract toolkit methods unless 100% identical semantics letter-by-letter.
  3. Tool Return Types (String vs DTO): tools consumed purely by the LLM return Markdown Strings; DTOs are reserved for data programmatically consumed by the framework.
  4. Shared `populateMessage` and `getSystemInstructions` in abstract toolkits.
  5. No defensive method-start null checks (fail fast).

### C. Live Diagnostic Annex in `IdeHandle` (`uno.anahata.asi.ide.resources.handle.IdeHandle`)
- Added `public List<HintInfo> getHints()` querying the session's active `AbstractHints` toolkit.
- Updated `public List<String> getAnnex()`:
  * Automatically formats and appends active inspection warnings and errors under the file header for all managed text resources via `HintInfo.toMarkdown(hints)`.
  * Omits redundant file name repeating since the resource header already establishes file identity.
  * Displays file diagnostics alongside working-copy VCS diffs and recent commit history with zero prompt bloat when clean.

### D. NetBeans `Hints.java` Modernization & Prefix KV-Cache Architecture
- Refactored `uno.anahata.asi.nb.tools.java.Hints` to `extends AbstractHints`:
  * Implements `getFileHints(String filePath) -> List<HintInfo>`.
  * Deleted legacy inner `HintInfo` and `HintMetadata` classes.
  * Added `setHintsEnabled(List<String> hintIds, boolean enabled)` tool to batch toggle multiple rules on/off in `HintsSettings`.
- **Prefix KV-Cache Optimization for Inspection Profile**:
  * Shifted the comprehensive 313-rule inspection catalog from dynamic tail RAG message (`populateMessage`) to static prefix system instructions (`getSystemInstructions()`) via `public String getHintMetadata()`.
  * Formatted as clean per-category Markdown tables (`### category` $\to$ `| Enabled | ID | Short Description |`).
  * Enabled rules listed first with `✅`, disabled rules listed second with blank space, both sorted alphabetically.
  * Evaluated dynamically on each turn (~15-20 ms) with zero static caching, guaranteeing 100% reactivity to manual user toggles in the NetBeans GUI while maximizing LLM prefix KV cache hits.

