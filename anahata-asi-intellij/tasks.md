# Anahata ASI IntelliJ Plugin — Active Tasks

This document tracks active development tasks, architecture refinements, and roadmap priorities for the `anahata-asi-intellij` module.

---

## 1. Active Focus & Tasks

### Task 1: Unify Line Comments & AI Commentary across all Text Write DTOs
- **Status**: [COMPLETED]
- **Description**: Push down `calculateLineComments(Agi)` to `AbstractTextResourceWrite` in `anahata-asi-core` so `TextResourceReplacements`, `TextResourceLineEdits`, `FullTextResourceUpdate`, and `CodeRefinementBatch` compute their AI commentary uniformly across both NetBeans and IntelliJ.
- **Key Enhancements**:
  - Moved `DiffCommentUtils` to `anahata-asi-core` under `uno.anahata.asi.toolkit.resources.text`.
  - Implemented `calculateLineComments(Agi)` on all core write DTOs.
  - Updated `IntellijTextResourceWriteRenderer` to delegate directly to `update.calculateLineComments(agi)`.
  - Updated `CommentGutterRenderer` in IntelliJ to render the authentic 16x16 Anahata logo (`AnahataFileIconProvider.getFileIcon()`) in the editor glyph gutter with the comment tooltip on hover.

### Task 2: Deduplicate `BatchCodeRefiner` and `CodeRefinementBatch` (IntelliJ)
- **Status**: [COMPLETED]
- **Description**: Eliminate ~145 lines of duplicate member-parsing, search, anchor-resolution, and insertion methods between `BatchCodeRefiner.java` and `CodeRefinementBatch.java` in `anahata-asi-intellij`.
- **Key Enhancements**:
  - Centralized PSI AST mutation helpers inside `CodeRefinementBatch.applyIntentToPsi(...)`.
  - Made `BatchCodeRefiner.refine()` delegate cleanly to `CodeRefinementBatch`.
  - Added `SmallTestClass.java` to `uno.anahata.asi.intellij.tools.java.coderefiner` for parity with NetBeans test coverage.

### Task 3: Interactive Diff Rendering for `CodeRefinementBatch`
- **Status**: [COMPLETED]
- **Description**: Ensure `CodeRefinementBatch` is registered in `ParameterRendererFactory` so that invoking `BatchCodeRefiner.refine(...)` displays an interactive, editable side-by-side diff panel with live write-back before execution.
- **Key Enhancements**:
  - Registered `CodeRefinementBatch.class` in `IntellijAsiContainer.initEnvironment()`.

### Task 4: Structured Test Results in `RunConfigurations`
- **Status**: [PENDING]
- **Description**: Attach an `SMTRunnerEventsListener` or `AbstractTestProxy` listener to the test execution process so `RunConfigurations.runConfiguration(...)` returns structured test summaries (tests run, passed, failed, ignored, duration, failure stack traces) instead of just the process exit code.

---

Força Barça!
