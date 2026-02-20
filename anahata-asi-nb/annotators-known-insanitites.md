
**Every Unexpected behavoiur quirk should be recorded here**


## 1. The Core Conflict
NetBeans has two distinct pipelines for decorating the same set of physical files:
- **Logical View (Projects Tab)**: Uses `ProjectIconAnnotator`. Authority: `AnahataProjectIconAnnotator`. Input: `Project` object.
- **Physical View (Files/Favorites Tabs)**: Uses `AnnotationProvider`. Authority: `FileAnnotationProvider`. Input: `Set<FileObject>`.

## 2. Technical Findings (From NetBeans Sources)
Studying `AnnotationProvider` and `BaseAnnotationProvider` revealed the following:
- **Replacement Model**: The pipeline for names and icons is a "First Responder Wins" or "Replacement" model. If a provider returns a non-null value, it terminates the chain for that specific file cluster.
- **HTML Parameter Constraint**: The `name` parameter passed to `annotateNameHtml` is **guaranteed to be plain text** (no HTML tags, just escaped characters). This means providers *cannot* see what previous providers in the chain have done unless they manually delegate.
- **Recursive Trap**: Because multiple providers might try to delegate to each other, a simple `Lookup.lookupAll` loop leads to a `StackOverflowError` (as seen in Round 3).

## 3. The "Hybrid" Pain Points
A "Hybrid" node is a Project Root folder.
- **In Projects Tab**: It is a `Project` node. NetBeans handles the Git branch name (e.g. `[main]`) as part of the logical node display.
- **In Files/Favorites Tabs**: It is just a folder (`FileObject`). NetBeans applies Git branch names via the `AnnotationProvider` pipeline.
- **Detection**: To know which view is being annotated inside `FileAnnotationProvider`, we use `StackWalker` to check if the caller is `org.netbeans.modules.project.ui`.

## 4. Round-by-Round Log

### Round 1-3: Interception & Delegation
- **What we tried**: Manual delegation in `FileAnnotationProvider` by looking up all `AnnotationProvider`s and calling them recursively.
- **What worked**: We successfully picked up Git icons and branch names.
- **What didn't work**: It was architecturally fragile.
- **Regressions**: **JVM CRASH.** Created an infinite recursion loop in the NetBeans lookup system, leading to a `StackOverflowError`.

### Round 4-6: Active Cleanup & Unified Indexing
- **What we tried**: Stripping our own `<font>` tags to prevent "stale" labels. Removing manual delegation to fix the crash.
- **What worked**: The Context tab became clean. Stable JVM.
- **What didn't work**: Git branch names and status colors (blue/red) disappeared because we were "terminating" the pipeline early with our own HTML.
- **Regressions**: No Git info in Physical views.

### Round 7: The Safe Master Aggregator (Current)
- **What we tried**: Moving to position 10000. Implementing a **ThreadLocal Guard** for safe manual delegation.
- **What worked**: TBD (Awaiting reload).
- **Strategy**: By running last (position 10000) and using a `DELEGATING` guard, we can safely call all other providers to get the "Base" state (Git/SVN), and then append our session labels to that base.
- **Hybrid Detection**: Improved `isProjectRoot` logic to allow badging in Physical views (Files/Favorites) while skipping in Logical views (Projects).

## Round 10.1: The "Messi" Fix (Stability)
- **Authority**: 
    - `AnahataProjectIconAnnotator` is the **exclusive** authority for Project nodes in the Projects tab.
    - `FileAnnotationProvider` **must return null** for Project Roots to prevent duplicate/incorrect labeling (like `(messi)` on project nodes).
- **Git Preservation**: 
    - `FileAnnotationProvider` now uses `delegateNameHtml` with a `ThreadLocal` guard to retrieve Git info before appending session labels.
    - Positions remain at 2000 (Projects) and 2100 (Files) to ensure we run AFTER Git but before the platform finishes.

## 5. Formatting Spec (Round 10 Unified)

### A. Counting & Ordering Logic
- **Unified Totals**: For a folder or project node, the count for each session is the **SUM** of all its context providers (Overview, anahata.md) plus all its resources (files).
- **Session Ordering**: Brackets MUST follow the exact order of `AsiContainer.getActiveChats()`.

Other nb sources involved in the annotation process

org.netbeans.modules.versioning.masterfs.VersioningAnnotationProvider
org.netbeans.modules.versioning.core.VersioningAnnotationProvider
org.netbeans.modules.versioning.core.filesystems.VCSFilesystemInterceptor
org.netbeans.modules.parsing.ui.indexing.errors.ErrorAnnotator

---
*Last Updated: 2026-02-11 23:14 (Round 7 Safe Aggregator)*
