# NetBeans Annotation Pipeline Strategy (`annotators.md`)

## 1. The Core Conflict
NetBeans has two distinct pipelines for decorating the same set of physical files:
- **Logical View (Projects Tab)**: Uses `ProjectIconAnnotator`. Authority: `AnahataProjectIconAnnotator`. Input: `Project` object.
- **Physical View (Files/Favorites Tabs)**: Uses `AnnotationProvider`. Authority: `FileAnnotationProvider`. Input: `Set<FileObject>`.

## 2. The "Hybrid" Pain Points
A "Hybrid" node is a Project Root folder.
- **In Projects Tab**: It is a `Project` node. NetBeans handles the Git branch name (e.g. `[main]`) as part of the logical node display.
- **In Files/Favorites Tabs**: It is just a folder (`FileObject`). NetBeans applies Git branch names via the `AnnotationProvider` pipeline.
- **Detection**: To know which view is being annotated inside `FileAnnotationProvider`, we use `StackWalker` to check if the caller is `org.netbeans.modules.project.ui`.

## 3. Round-by-Round Log

### Round 1-3: Interception & Delegation
- **What we tried**: Manual delegation in `FileAnnotationProvider` by looking up all `AnnotationProvider`s and calling them recursively.
- **What worked**: We successfully picked up Git icons and branch names.
- **What didn't work**: It was architecturally fragile.
- **Regressions**: **JVM CRASH.** Created an infinite recursion loop in the NetBeans lookup system, leading to a `StackOverflowError`.

### Round 4-5: Active Cleanup & Unified Indexing
- **What we tried**: Stripping our own `<font>` tags to prevent "stale" labels. Removing manual delegation to fix the crash.
- **What worked**: The Context tab became clean. Stable JVM.
- **What didn't work**: Git branch names and status colors (blue/red) disappeared.
- **Regressions**: Returning a non-null value from `annotateNameHtml` without delegation makes our provider the "terminator" of the pipeline, effectively hiding Git info.

### Round 6: Numbered Aggregator (In Progress)
- **What we tried**: Unified counting (Providers + Resources). Ordered brackets `[sum1][sum2]`. (nickname) for files.
- **What worked**: Unified UI in the Context tab.
- **What didn't work**: Still missing Git info in tooltips/names because we aren't "re-injecting" the system annotations correctly.

## 4. Round 6 Refined Spec

### A. Counting & Ordering Logic
- **Unified Totals**: For a folder or project node, the count for each session is the **SUM** of all its context providers (Overview, anahata.md) plus all its resources (files).
- **Session Ordering**: Brackets MUST follow the exact order of `AsiContainer.getActiveChats()`.

### B. Naming Convention
- **Files Only**: Use **Round Brackets** `()`.
  - format: `(displayName)` if in **exactly one** session. `displayName` is `Chat.getDisplayName()` (nickname or short ID). E.g., `(messi)`.
  - format: `(n)` if in **multiple** sessions. E.g., `(2)`.
- **Folders / Project Nodes**: Use **Square Brackets** `[]`.
  - format: `[sum1][sum2]...` (one bracket per active session).
  - **CRITICAL**: Projects MUST show square brackets, even when they appear as folders in the Files tab.

### C. Name Annotation Logic (`annotateNameHtml`)
- **Preservation**: Capture the incoming `name` (which contains Git HTML like `[main]`) and append our label before `</html>`.
- **Active Cleanup**: Always strip previous `<font color='#707070'>` labels to prevent stale nicknames.

---
*Last Updated: 2026-02-11 22:45 (Round 6 Refined)*
