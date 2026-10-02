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
