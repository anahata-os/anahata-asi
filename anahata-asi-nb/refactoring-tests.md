# Refactoring Test Suite (V2)

This file tracks the progress and results of the refactoring tools implemented in the V2 plugin.

## Test Log

| Date | Tool | Target | Result | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 2026-02-04 | `rename` | `RenamedTestProject.java` | SUCCESS | Renamed to `RefactoredTestProject.java` in `reload-test-project`. |
| 2026-02-04 | `whereUsed` | `RefactoredTestProject.java` | SUCCESS | No usages found (expected). |
| 2026-02-04 | `whereUsed` | `Refactor.java` | SUCCESS | No usages found (it's a new tool class). |
| 2026-02-04 | `whereUsed` | `AnahataTopComponent.java` (v1) | FAIL | Returned "No usages found". Debugging: Improved lookup to include `ElementHandle`. |
| 2026-02-04 | `replaceInTextFile` | `RefactoredTestProject.java` | SUCCESS | Replaced "HOT RELOAD" and "Hello World!" in one call. |
| 2026-02-04 | `whereUsedMember` | `RefactoredTestProject.getMessage` | SUCCESS | Found 0 usages (correct after replacement). Verified with `ElementHandle` fix. |
| 2026-02-04 | `move` | `RefactoredTestProject.java` | SUCCESS | Moved to `uno.anahata.reload.test.moved` package. |
| 2026-02-04 | `move` (Cross-Project) | `RefactoredTestProject.java` | FAIL | Reported SUCCESS but file remained at source. Likely not supported by `MoveRefactoring`. |
| 2026-02-04 | `copy` | `RefactoredTestProject.java` | FAIL | Encountered `NullPointerException` in `URLMapper.findFileObject`. Debugging: Target folder resolution issue. |
| 2026-02-04 | `safeDelete` | `RefactoredTestProject.java` | PENDING | Testing safety checks. |

## Pending Tests
- [ ] `inline`
- [ ] `encapsulateField`
- [ ] `invertBoolean`
- [ ] `extractInterface`
- [ ] `pullUp`
- [ ] `pushDown`
