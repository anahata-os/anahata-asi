# NetBeans Annotation Pipeline Strategy (`annotators.md`)

## 1. The Core Conflict
NetBeans has two pipelines for the same physical files:
- **Logical View (Projects Tab)**: Uses `ProjectIconAnnotator`. Authority: `AnahataProjectIconAnnotator`.
- **Physical View (Files/Favorites Tabs)**: Uses `AnnotationProvider`. Authority: `FileAnnotationProvider`.

## 2. The "Round" History
- **Round 1-3**: Interception, Manual Delegation, and Active Cleanup.
- **Round 4-5**: Unified indexing and session ordering.
- **Round 6**: Refining brackets and session display.

## 3. Round 6 Spec: "Numbered Aggregator"

### A. Counting & Ordering Logic
- **Unified Totals**: For a folder or project node, the count for each session is the **SUM** of all its context providers plus all its resources.
- **Session Ordering**: Square brackets `[count1][count2]` follow the `AsiContainer.getActiveChats()` order.

### B. Naming Convention
- **Files Only**: Use **Round Brackets** `()`.
  - format: `(n) [nickname]` where `n` is the number of sessions.
  - If 1 session: `(1) messi`.
  - If > 1 session: `(n)` (e.g., `(2)`). (Omit names if ambiguous).
- **Folders / Project Nodes**: Use **Square Brackets** `[]`.
  - format: `[sum1][sum2]...` (one bracket per active session).
  - **CRITICAL**: Projects MUST show square brackets, even if they appear as data files in some views.

### C. Name Annotation Logic (`annotateNameHtml`)
- **Git Preservation**: Ensure the incoming `name` (which may contain Git HTML) is preserved and the label is appended before `</html>`.
- **Surgical Cleanup**: Strip previous `<font>` labels to prevent stale nicknames.

### D. Known Issues (Round 5)
- **Git Info Missing**: Git annotations and tooltips disappeared from both Projects and Files views.
- **Stale Tooltips**: Tooltips occasionally missing or duplicated.
- **Project Refresh**: Lag in updating project nodes in the Files view.

---
*Last Updated: 2026-02-11 22:18 (Round 6: Numbered Aggregator)*
