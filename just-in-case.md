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
