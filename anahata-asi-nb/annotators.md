# Annotators Strategy & Failed Attempts

## Current Strategy
1.  **Project Badges**: Handled exclusively by `AnahataProjectAnnotator` (v2) in `anahata-asi-nb`.
    -   Position: Offset 16 (to the right of the 16x16 icon).
    -   Condition: Only if at least one active chat has the project provider for that project "providing".
    -   Handshake: Sets the `uno.anahata.asi.badged` attribute on the project root `FileObject` to signal that it has handled the annotation.
2.  **File Badges**: Handled by `FileAnnotationProvider` (v2) in `anahata-asi-nb`.
    -   Scope: Individual files only.
    -   Handshake: Skips any `FileObject` that has the `uno.anahata.asi.badged` attribute set.
    -   Position: **Offset 8** (top-right). This is the correct position for file badges to avoid clashing with Git's bottom-right badges.
    -   **Good Citizen HTML**: In `annotateNameHtml`, it surgically injects the `[n]` label before the closing `</html>` tag using regex. This preserves existing HTML annotations (like Git colors and branch info).
    -   Status: Active.

## Node Factory Clarification (V1 vs V2)
-   **V1 `AnahataNodeFactory`**: Creates a virtual "Anahata" folder in the Projects tab and sets the primary icon for `anahata.md`. This is structural node creation, not annotation.
-   **V2 Strategy**: We do **not** have a Node Factory yet. `anahata.md` is treated as a standard file and only receives an Anahata **badge** (at offset 8) if it is actually in the AI context. We must not force a primary icon on it based solely on its name.

## Failed Attempts
-   **Attempt 1**: Using `AnnotationProvider` for both files and projects.
    -   *Result*: Duplicated badges on projects because `ProjectIconAnnotator` was also active.
-   **Attempt 2**: Using `VersioningSupport.fireFileStatusChanged` to force refreshes.
    -   *Result*: Deemed "dirty and dodgy". Potentially interfered with standard NetBeans error badges and `AnahataNodeFactory`.
-   **Attempt 3**: Using `SwingUtilities.invokeLater` with a static listener list in `AnahataProjectAnnotator`.
    -   *Result*: Still reported as "double icons" after reload.
-   **Attempt 4**: Changing file badge offset to 16.
    -   *Result*: Cancelled by user. Offset 8 is the correct and desired position for file-level badges.
-   **Attempt 5**: Forcing Anahata primary icon on `anahata.md` in `FileAnnotationProvider`.
    -   *Result*: Incorrectly mixed structural node decoration with annotation. Reverted.
-   **Attempt 6**: `isProjectRoot` failing due to `IllegalArgumentException` when calling `findProject` on a file.
    -   *Result*: Fixed by adding `fo.isFolder()` check and using `FileUtil.toFile()` for robust path comparison.
-   **Attempt 7**: Tooltip-based double-annotation checks.
    -   *Result*: Deemed "dirty and hacky". Replaced by the `FileObject` attribute handshake.
-   **Attempt 8**: Manual `VersioningAnnotationProvider` call in `annotateNameHtml`.
    -   *Result*: Trashed Git annotations (colors and branch info). Replaced by surgical HTML injection.

## Known Issues
-   NetBeans error badges (red marks) are disappearing or not showing correctly.
