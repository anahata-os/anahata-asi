# Annotators Strategy & Failed Attempts

## Current Strategy
1.  **Project Badges**: Handled exclusively by `AnahataProjectAnnotator` (v2) in `anahata-asi-nb`.
    -   Position: Offset 16 (to the right of the 16x16 icon).
    -   Condition: Only if at least one active chat has the project provider for that project "providing".
2.  **File Badges**: Handled by `FileAnnotationProvider` (v2) in `anahata-asi-nb`.
    -   Scope: Individual files only (skip project roots).
    -   Status: **Temporarily Disabled** (commented out) to isolate project badge issues.

## Failed Attempts
-   **Attempt 1**: Using `AnnotationProvider` for both files and projects.
    -   *Result*: Duplicated badges on projects because `ProjectIconAnnotator` was also active.
-   **Attempt 2**: Using `VersioningSupport.fireFileStatusChanged` to force refreshes.
    -   *Result*: Deemed "dirty and dodgy". Potentially interfered with standard NetBeans error badges and `AnahataNodeFactory`.
-   **Attempt 3**: Using `SwingUtilities.invokeLater` with a static listener list in `AnahataProjectAnnotator`.
    -   *Result*: Still reported as "double icons" after reload (likely due to `FileAnnotationProvider` still being active).

## Known Issues
-   NetBeans error badges (red marks) are disappearing or not showing correctly.
-   `AnahataNodeFactory` (v1) node decorations are broken.
