# Anahata ASI Session Summary & Architecture Handover (2026-10-05)

## 1. Executive Summary
This session addressed and permanently resolved critical architectural issues across the multi-module codebase, spanning Kryo session serialization/deserialization, IntelliJ background inspection concurrency deadlocks, quick-fix headless execution governance, and the complete clean-room design and implementation of **`CodeModel`** (a universal, token-efficient JVM Code Model toolkit for IntelliJ IDEA).

---

## 2. Issue 1: Kryo Deserialization Crash on Session Import (`this.mutex` is null)

### A. Root Cause Analysis
- **Symptom**: Importing a previously saved session threw:
  ```text
  Caused by: com.esotericsoftware.kryo.KryoException: java.lang.NullPointerException: Cannot enter synchronized block because "this.mutex" is null
  Serialization trace:
  phases (uno.anahata.asi.ide.tools.maven.MavenBuildResult)
  result (uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse)
  ...
  Caused by: java.lang.NullPointerException: Cannot enter synchronized block because "this.mutex" is null
      at java.base/java.util.Collections$SynchronizedCollection.add(Collections.java:2332)
      at com.esotericsoftware.kryo.serializers.CollectionSerializer.read(CollectionSerializer.java:241)
  ```
- **Mechanism**:
  1. In `IntellijMaven.java` (line 335), `phases` was initialized as `Collections.synchronizedList(new ArrayList<>())` to allow safe concurrent event population from IntelliJ's `ProcessListener` callbacks.
  2. `Collections.synchronizedList(...)` returns an instance of `java.util.Collections$SynchronizedRandomAccessList` (subclass of `SynchronizedCollection`).
  3. This class wraps an underlying collection `c` and delegates operations inside a synchronized block protected by `final Object mutex`.
  4. Neither `Kryo` nor `JdkCollectionsSerializers` registered a dedicated serializer for `Collections.synchronizedList`.
  5. Kryo defaulted to its generic `CollectionSerializer`. Because `SynchronizedRandomAccessList` has no public no-arg constructor, Kryo invoked Objenesis (`StdInstantiatorStrategy`) to allocate memory directly without executing any constructor.
  6. As a result, `this.mutex` initialized to `null`. When `CollectionSerializer.read` called `collection.add(kryo.readClassAndObject(input))`, the JVM attempted to synchronize on `this.mutex`, throwing an immediate `NullPointerException`.

### B. Kryo Source Code Audit & Redundancy Removal
- By inspecting the source code of `Kryo.java`, `DefaultSerializers.java`, and `ImmutableCollectionsSerializers.java` via `CodeModel`, we discovered that our custom `JdkCollectionsSerializers.java` in `anahata-asi-core` had duplicated 400+ lines of serializers already bundled inside Kryo:
  * `ImmutableListSerializer`, `ImmutableSetSerializer`, `ImmutableMapSerializer` $\to$ already implemented in `ImmutableCollectionsSerializers`.
  * `ArraysListSerializer` $\to$ already implemented in `DefaultSerializers.ArraysAsListSerializer`.
  * `SingletonListSerializer`, `SingletonSetSerializer`, `SingletonMapSerializer` $\to$ already implemented in `DefaultSerializers.CollectionsSingleton*`.
  * `EmptyListSerializer`, `EmptySetSerializer`, `EmptyMapSerializer` $\to$ already implemented in `DefaultSerializers.CollectionsEmpty*`.
- **Cleanup**: Deleted all 400+ lines of redundant serializers and replaced them with official Kryo registration:
  ```java
  ImmutableCollectionsSerializers.registerSerializers(kryo);
  ```

### C. Dedicated Synchronized Collection Serializers in Core
- Implemented dedicated, thread-safe Kryo serializers for the entire `Collections.synchronized*` family in `uno.anahata.asi.persistence.kryo.JdkCollectionsSerializers`:
  * `SynchronizedCollectionSerializer`
  * `SynchronizedListSerializer` (preserves `RandomAccess` vs `LinkedList`)
  * `SynchronizedSetSerializer`
  * `SynchronizedMapSerializer`
- **Thread Safety**: All serializers synchronize on `col`, `list`, `set`, or `map` during `write(...)` and `copy(...)` to prevent `ConcurrentModificationException` during serialization pulses.
- **Wire Compatibility & Deserialization**:
  * Instead of populating an uninitialized Objenesis instance, `read(...)` reads the elements into a clean collection (`ArrayList`, `LinkedHashSet`, `LinkedHashMap`) and returns `Collections.synchronized*(...)`. This runs the authentic JDK constructor, setting `this.mutex = this`.
  * Because the wire format (`[size][element...]`) matches Kryo's default `CollectionSerializer` 1:1, **existing session files on disk that failed to import can now be loaded cleanly without data loss!**
- **Unmodifiable & Empty Collections**:
  * Added `UnmodifiableCollectionSerializer`.
  * Preserved the 4 sorted/navigable empty serializers that Kryo lacks (`emptyNavigableSet`, `emptyNavigableMap`, `emptySortedSet`, `emptySortedMap`).

### D. Result DTO Hygiene in `IntellijMaven.java`
- In `IntellijMaven.java` (line 442), when returning `MavenBuildResult`, `phases` is now synchronized and copied into a standard, unsynchronized snapshot:
  ```java
  List<MavenBuildResult.BuildPhase> finalizedPhases;
  synchronized (phases) {
      finalizedPhases = new ArrayList<>(phases);
  }
  return new MavenBuildResult(status, exitCode, stdOutput, stdError, logFilePath, finalizedPhases);
  ```
  Result DTOs crossing process or serialization boundaries should never retain thread-synchronized wrappers once execution has completed.

---

## 3. Issue 2: IntelliJ Hints Concurrency Deadlock & Architecture Overhaul

### A. Thread Dump Forensics (`threadDump-20261005-123611-1791196571150.txt`)
- **The Problem**: Opening or assembling a turn with multiple files in context caused the IDE to freeze completely for 30+ seconds or deadlock.
- **Forensic Execution Trace**:
  1. `ContextManager.buildRagMessage()` executes context providers in parallel via `CompletableFuture`.
  2. Each file resource in context executed `IdeHandle.getAnnex()` $\to$ `getHints()` $\to$ `IntellijHints.getFileHints()`.
  3. `IntellijHints.getFileHints()` called `DaemonCodeAnalyzerImpl.runMainPasses(psiFile, document, progress)`.
  4. 12 worker threads (`thread-28`, `thread-30`, `thread-31`, etc.) concurrently entered `runMainPasses`.
  5. Inside `runMainPasses`:
     - Line 495: `myFileStatusMap.markAllFilesDirty("prepare to run main passes")` and `myPassExecutorService.cancelAll(true)`. Every thread cancelled every other thread's passes in an infinite loop!
     - `ExternalToolPass.getInfos` called `TimeoutUtil.sleep(18)` in a loop holding active `ReadAction`s.
  6. The Event Dispatch Thread (`AWT-EventQueue-0`) attempted `ApplicationImpl.runWriteAction`, which blocked waiting for all active `ReadAction`s to release.
  7. In IntelliJ's locking model, pending write locks prevent new read locks from being granted (`smartAcquireReadPermit` suspended worker threads).
  8. Context providers needing the EDT (`Editor.populateMessage` on `thread-27` and `OpenToolWindowsContextProvider` on `thread-9`) called `invokeAndWait`, blocking on the EDT.
  9. **Result**: Total deadlock between the EDT, the daemon, and background AI threads.

### B. Caching Strategy Decision: 100% Native Truth
- External in-memory caching of hints (e.g. `(filePath, modificationStamp) -> hints`) was rejected because in Java, **inspections and dataflow analysis cross file boundaries**:
  * Modifying `FileA.java` (e.g. changing `@Nullable` to `@NotNull` on a method) introduces new inspection warnings ("unnecessary null check") in `FileB.java`, even though `FileB` was never touched and its modification stamp never changed.
  * Only IntelliJ's native `FileStatusMap` and `PsiModificationTracker` track cross-file dirty scopes accurately.
- Therefore, we rely **exclusively on IntelliJ's native highlighting pipeline**.

### C. The Two-Tier Non-Blocking Architecture (`IntellijHints.java`)
Implemented a robust two-tier architecture in `IntellijHints.java`:
1. **Tier 1: Native Cache Guarantee (0 ms Fast Path)**:
   - Queries `analyzer.isAllAnalysisFinished(psiFile)`.
   - This natively verifies:
     * `document != null && psiDocumentManager.isCommitted(document)`
     * `document.getModificationStamp() == psiFile.getViewProvider().getModificationStamp()`
     * `myFileStatusMap.allDirtyScopesAreNullFor(document) == true`
   - If `true`, all passes have run to completion and results are mathematically guaranteed fresh. Highlights are extracted directly from `DocumentMarkupModel` via `DaemonCodeAnalyzerEx.processHighlights(...)` in **0 to 1 ms** with zero pass execution and zero lock contention.
2. **Tier 2: Active Daemon Synchronization (Bounded Wait for Open Files)**:
   - If analysis is not finished and the file is open in an editor (`FileEditorManager.isFileOpen(vf)`), the background timer daemon is already running on it.
   - Polls `isAllAnalysisFinished(psiFile)` in a polite wait loop (up to 2000 ms) with `ProgressManager.checkCanceled()` and `Thread.sleep(30)`.
   - Once complete, reads from `DocumentMarkupModel` in 0 ms.
3. **Tier 3: Safe On-Demand Execution for Closed Files**:
   - IntelliJ's background timer daemon never runs on closed files.
   - If the file is not open in an editor, it breaks out of the wait loop immediately (0 ms) and executes on-demand passes under a single-threaded mutex (`ON_DEMAND_ANALYSIS_LOCK`).
   - **Critical Safety Guards**:
     * **No Global Sabotage**: Does NOT call `runMainPasses`! Instead, instantiates passes for that file only via `TextEditorHighlightingPassRegistrarEx.instantiateMainPasses(...)`.
     * **No Sleep Loops**: Filters out `ExternalToolPass` to eliminate the `TimeoutUtil.sleep(18)` loop.
     * **Status Update**: Marks the file clean via `analyzer.getFileStatusMap().markFileUpToDate(...)`.
4. **EDT Assertion Fix**:
   - Replaced `analyzer.isRunningOrPending()` (which throws `assertEventDispatchThread()`) with `FileEditorManager.getInstance(project).isFileOpen(fileVf)`, ensuring 100% thread safety when invoked from background AI tool threads.

---

## 4. Issue 3: Quick-Fix Execution Badges (`(⚡)` Headless vs. `(👤)` Interactive)

### A. Root Cause: Why "Save to dictionary" Failed Headlessly
- When testing `applyHint(filePath, 1, "Save 'Força' to dictionary")`, the typo warning remained in the file.
- Inspecting `SpellCheckerManager.getInstance(project).getUserDictionaryWords()` revealed it returned `[]`.
- The fix class was `com.intellij.grazie.ide.inspection.grammar.quickfix.GrazieCustomFixWrapper`. In Grazie, "Save to dictionary" is an interactive UI action that opens a popup menu asking the user whether to save to the Project dictionary or Application dictionary. When invoked headlessly via `fix.invoke(...)`, the popup cannot display, so it aborts silently without modifying the dictionary.
- Conversely, testing `applyHint(filePath, 64, "Replace with text block")` (a `ModCommandActionWrapper`) executed cleanly inside `WriteCommandAction` on the EDT in 304 ms, transforming the file AST immediately.

### B. Dynamic Badging in File Footers
- IntelliJ inspections do not declare their quick-fixes statically in the tool catalog; quick-fixes are instantiated dynamically at runtime inside `ProblemsHolder.registerProblem(...)`.
- We evaluate quick-fixes when reading `HighlightInfo`s in `IntellijHints.getFileHints`:
  * **`(⚡)` Headless**: Direct code transformations (`ModCommandAction`, `action.startInWriteAction() == true`). Runnable programmatically via `applyHint` in a single turn.
  * **`(👤)` Interactive**: Actions requiring user UI interaction (Grazie, dictionary choices, settings/options dialogs, chooser popups, refactoring parameter dialogs, `startInWriteAction() == false`).
- **Fail-Fast Protection**: If `applyHint` is called on a fix tagged with `(👤)`, it immediately throws an `AgiToolException` explaining that the fix requires user interaction in the IDE UI.
- **Input Sanitization**: `applyHint` automatically strips `(⚡)` and `(👤)` from the requested fix name, allowing the AI to pass either the raw or tagged string.

### C. Architectural Boundary Enforcement (`anahata-asi-ide/anahata.md`)
- **Anti-Pattern Corrected**: Initially, badge instructions were placed in `AbstractHints.java`. This was identified as a violation of Rule 1 ("Strictly Zero False Abstractions & 100% Preservation of Native IDE Semantics"): NetBeans (`NbHints`) applies fixes by catalog rule ID (`applyHintFix`), not line-bound `IntentionAction`s, and does not have `(⚡)`/`(👤)` badges.
- `AbstractHints.java` was reverted, badge instructions were placed strictly in `IntellijHints.java` and `IntellijVCS.java`, and the cautionary anti-pattern was reinforced in `anahata-asi-ide/anahata.md`.

---

## 5. Issue 4: Clean-Room `CodeModel` Universal JVM Architecture

### A. NetBeans Port Flaws Identified
Comparing `uno.anahata.asi.nb.tools.java.CodeModel` with `uno.anahata.asi.intellij.tools.java.CodeModel` revealed extensive legacy baggage:
1. **The Vestigial `URL` Artifact**: NetBeans Javac resolution is URL-based (`URLMapper.findFileObject(url)`). Because `JavaMember extends JavaType`, every member inherited a `URL` field, burning thousands of tokens. In IntelliJ, `URL` had to be faked from `VirtualFile.getPath()`.
2. **The 14-Tool Duplicate Syndrome**: To support optimistic shortcuts vs. URL keychains, every operation had two duplicate tools: `getMembers` vs `getMembersByFqn`, `getSource` vs `getMemberSourcesByFqn`, `getJavadoc` vs `getMemberJavadocsByFqn`, `getSubtypes` vs `getSubtypesByFqn`, `getSupertypes` vs `getSupertypesByFqn`, `loadTypeSources` vs `loadTypeSourcesByFqn`.
3. **Naive Class Search**: `findTypes` was iterating through an array of 50,000 class names with regex in a manual Java loop.
4. **Hierarchy Bloat**: `JavaHierarchyNode` was serializing empty lists and nested URL keychains for every node in the tree.

### B. The Ambiguity Solution: `fqn` + Optional `location`
- Duplicate FQNs are a genuine reality in multi-module workspaces (different JDK versions, conflicting library versions, test vs main).
- Instead of maintaining two parallel sets of tools, `CodeModel` uses **ONE single tool per capability** with an optional `location` parameter:
  * **99% Case**: Passing `fqn` alone resolves immediately.
  * **Ambiguous Case**: If multiple classes share the FQN, the tool fails fast with a list of matching locations (e.g. `Module: anahata-asi-core`, `SDK: 25`, `Library: kryo-5.6.2.jar`). The caller provides `location` to resolve deterministically.

### C. Universal JVM Language Support
- IntelliJ unifies all JVM languages through `com.intellij.psi.PsiClass`:
  * **Java**: `PsiClassImpl`
  * **Kotlin**: `KtLightClass`
  * **Groovy**: `GrTypeDefinition` (extends `PsiClass` directly)
  * **Scala**: `ScTypeDefinition`
- Source code extraction via `cl.getNavigationElement().getContainingFile()` resolves authentic native source text across Java (`.java`), Kotlin (`.kt`), Groovy (`.groovy`), and Scala (`.scala`).

### D. New Clean-Room Package (`uno.anahata.asi.intellij.tools.java.codemodel2`)
Implemented 6 clean models and the unified toolkit:
1. **`CodeModel2.java`**: 7 unified tools:
   - `findTypes(query, caseSensitive, startIndex, pageSize)`: Uses `PsiShortNamesCache` and `JavaPsiFacade` with camel-hump and glob wildcard matching.
   - `getMembers(fqn, location, nameQuery, kindFilters, includeInherited, startIndex, pageSize)`: Clean member listing.
   - `getSource(targetFqn, location)`: Unified source retrieval for both types and members.
   - `getJavadoc(targetFqn, location)`: Unified documentation retrieval for both types and members.
   - `getHierarchy(fqn, direction, location, maxDepth)`: Unified tree traversal for either `SUBTYPES` or `SUPERTYPES`.
   - `loadSources(fqns, location)`: Loads project files or decompiled library sources as managed resources.
   - `findTypesInPackage(packageName, kindFilter, recursive, location, startIndex, pageSize)`: Package type discovery.
2. **`TypeItem.java`**: Pure data DTO (`fqn`, `simpleName`, `kind`, `language`, `location`).
3. **`MemberItem.java`**: Pure member DTO (`fqn`, `name`, `kind`, `language`, `signature`, `returnType`, `modifiers`, `isInherited`, `declaringClassFqn`).
4. **`MemberPage.java`**: Token-saving container carrying enclosing type metadata at the page level.
5. **`HierarchyDirection.java`**: Enum (`SUBTYPES`, `SUPERTYPES`).
6. **`HierarchyNode.java`**: Clean recursive node using a single `children` collection without empty list bloat.
7. **`package-info.java`**: ASI-grade package Javadoc.

---

## 6. Current Repository Status & Verification

### A. Modified & Untracked Files
- **`anahata-asi-core`**:
  * `JdkCollectionsSerializers.java`: Removed duplicate serializers; added synchronized and unmodifiable collection serializers; delegated to `ImmutableCollectionsSerializers.registerSerializers(kryo)`.
- **`anahata-asi-ide`**:
  * `anahata.md`: Reinforced Rule 1 with the quick-fix execution badges leak anti-pattern example.
  * `AbstractHints.java`: Kept pure without IntelliJ-specific execution badges.
- **`anahata-asi-intellij`**:
  * `IntellijMaven.java`: Finalized `phases` with unsynchronized snapshot list.
  * `IntellijHints.java`: Two-tier non-blocking architecture, `(⚡)`/`(👤)` quick-fix badges, `isFileOpen` thread safety fix.
  * `IntellijVCS.java`: Added quick-fix badge instructions.
  * `IntellijAgiConfig.java`: Registered `CodeModel2.class`.
  * `uno.anahata.asi.intellij.tools.java.codemodel2.*`: The new universal JVM Code Model toolkit and DTOs.

### B. Build & Reload Instructions
As documented in Rule 62:
```bash
mvn clean package -DskipTests
```
After packaging, click **Reload plugin** in the IntelliJ toolbar to activate `CodeModel` and all runtime updates.

Força Barça!
