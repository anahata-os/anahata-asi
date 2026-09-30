# Anahata ASI Session Backup: Just In Case

- **Export Time**: 2026-09-29T18:15:00+02:00
- **Session ID**: `2453cc36-aab4-4946-b059-dd9558877a74`
- **Session Nickname**: `Projects refactor`
- **Selected Model**: `gemini-3.8-flash`
- **Active AI Provider**: `GeminiGCExpress`
- **Build Status**: Verified Compile (0 Errors, 0 Alerts across ALL open projects: Parent, Core, NetBeans, IntelliJ, Swing UI)

---

## 1. Executive Summary: The Universal Projects & Scope Refactor

This session achieved a complete, unified overhaul of the project management domain, context provider architecture, and granularity scoping across all four primary modules (`anahata-asi-core`, `anahata-asi-nb`, `anahata-asi-intellij`, and `anahata-asi-swing`).

The architecture establishes:
1. Universal project contracts and scoping in `core`.
2. Fully symmetrical IDE toolkits and providers in `nb` (`NbProjects`, `NbVCS`, `NbProjectContextProvider`) and `intellij` (`IntellijProjects`, `IntellijVCS`, `IntellijProjectContextProvider`).
3. Reactive, non-blocking Swing UI with value-object immutability, background token calculations, and clean text wrapping.

---

## 2. Core Architectural Components (`anahata-asi-core`)

### A. `AbstractProjects.java` (`uno.anahata.asi.toolkit.project`)
- Universal base class for IDE project toolkits across all host environments.
- Holds workspace-wide default granularity: `protected ProjectStructureScope defaultScope = new ProjectStructureScope()`.
- Fires `"projectStructureScope"` property change event on `setDefaultScope(scope)`.
- Defines universal abstract contracts:
  * `List<String> getOpenProjects()`
  * `void closeProjects(List<String> projectPaths)`
  * `String openProject(String projectPath)`
  * `ProjectOverview getOverview(String projectPath)`
  * `Optional<? extends AbstractProjectContextProvider> getProjectProvider(String projectPath)`
- Implements shared `@AgiTool setProjectProviderEnabled(projectPath, enabled)` directly in `core`.
- Exposes new `@AgiTool setProjectStructureScope(projectPath, scope)` allowing the AI model to adjust granularity on the fly.

### B. `AbstractProjectContextProvider.java` (`uno.anahata.asi.toolkit.project`)
- Universal base class for both root project nodes and submodule nodes.
- Holds `projectPath`, `projectsToolkit`, and local `scope` (nullable for inheritance).
- **4-Tier Cascading Scope Resolution (`getEffectiveScope()`)**:
  1. Local `this.scope` override on the node (if non-null).
  2. Parent project `parent.getEffectiveScope()` (for submodules in hierarchical trees like IntelliJ).
  3. Toolkit-level default `projectsToolkit.getDefaultScope()` (workspace-wide default for flat topologies like NetBeans).
  4. Standard `new ProjectStructureScope()` fallback.
- **Immediate Parent Binding**: Constructor takes `ContextProvider parentProvider`, setting `this.parent = parentProvider` immediately to ensure `getAgi()` and `syncMdResource()` never encounter null references during initialization.
- **Centralized `extractFirstSentence(String docComment)`**:
  * Strips block comment symbols (`/**`, `*/`), asterisks (`*`), and Markdown markers (`///`, `//`).
  * Cleans Markdown links `[text](url)` $\to$ `text` and references `[text]` $\to$ `text`.
  * Removes legacy HTML tags and normalizes whitespace.
  * Shared single source of truth across both IDE plugins.

### C. `ProjectStructureScope.java` (`uno.anahata.asi.toolkit.project`)
- Value-object DTO annotated with `@Builder(toBuilder = true)`.
- Features 9 granular switches:
  * `showAlerts`: Compiler errors & diagnostic problems.
  * `showRootFiles`: Root directory files (`pom.xml`, `README.md`, `anahata.md`).
  * `showResources`: Resource folders (`src/main/resources`, `.github`).
  * `showVcsStatus`: VCS status badges (`[-/M]`, `[-/A]`).
  * `showFileSizes`: Physical file sizes (`[12.5 KB]`).
  * `showElementKind`: Java element kinds (`(CLASS)`, `(INTERFACE)`, `(ENUM)`, `(RECORD)`).
  * `showInnerClasses`: Inner and nested classes.
  * `showSupertypes`: Extended superclasses and implemented interfaces (`extends Foo implements Bar`).
  * `showJavadoc`: First sentence of class-level and package-level Javadocs.

### D. `ProjectOverview.java` (`uno.anahata.asi.toolkit.project`)
- Unified "identity card" DTO in `core`:
  * Project name, packaging type (`jar`, `pom`, `nbm`), and Maven coordinates (`groupId:artifactId:version`).
  * Java source/target levels and encoding.
  * Supported IDE actions list.
  * Declared Maven dependencies grouped by scope and groupId (`DependencyScope`).
  * Live Git repository overview (branch, remotes, modified files, recent commits) if repo root.

---

## 3. NetBeans Integration (`anahata-asi-nb`)

### A. Renamed & Modernized Toolkits
- **`NbProjects.java`**:
  * Extends `AbstractProjects` from `core`.
  * Inherits `defaultScope` and `setProjectStructureScope`.
  * In `syncProjects()`: instantiates flat `NbProjectContextProvider` instances for open projects.
  * Overrides `setProjectProviderEnabled` to trigger recursive visual icon refresh (`FilesContextActionLogic.fireRefreshRecursive`).
- **`NbVCS.java`**:
  * Renamed from `VCS.java`, extends `AbstractVCS`.
- **`NbProjectContextProvider.java`**:
  * Renamed from `GrandProjectContextProvider.java`, extends `AbstractProjectContextProvider`.
  * Emits Overview (with Git status if repo root), Alerts (if enabled in scope), and AST Structure in a single cohesive turn.
- **AST Domain Model Upgrade**:
  * `ProjectNode.renderMarkdown(sb, indent, scope)` standard applied across all 6 component classes (`ProjectStructure`, `JavaSourceGroup`, `JavaPackage`, `ProjectComponent`, `ResourceFolder`, `ResourceSourceGroup`).
  * `JavaSourceGroup`: Extracts `extends` and `implements` supertypes, plus first-line Javadoc summaries (supporting traditional Javadoc and modern JDK 23+ JEP 467 `///` Markdown comments).
  * Obsolete `boolean summary` completely purged.

### B. Legacy Cleanup
- Safely deleted legacy classes: `ProjectFiles.java`, `ProjectFile.java`, `SourceFolder.java`, and the duplicate NetBeans `ProjectOverview.java`.

---

## 4. IntelliJ Integration (`anahata-asi-intellij`)

### A. Renamed & Modernized Toolkits
- **`IntellijProjects.java`**:
  * Renamed from `Projects.java`, extends `AbstractProjects` from `core`.
  * Inherits `defaultScope` and `setProjectStructureScope`.
  * Implements `getOpenProjects()`, `closeProjects()`, `openProject()`, `getOverview()`.
  * `getProjectProvider(path)` traverses flattened hierarchy (`flatMap(gcp -> gcp.getFlattenedHierarchy(true).stream())`) resolving both root projects and submodules.
- **`IntellijVCS.java`**:
  * Renamed from `VCS.java`, extends `AbstractVCS`.
- **`IntellijProjectContextProvider.java`**:
  * Renamed from `GrandProjectContextProvider.java`, extends `AbstractProjectContextProvider`.
  * Polymorphic dual constructors:
    1. Root project constructor (`module = null`): Discovers submodules via `syncModules()`.
    2. Submodule constructor (`module != null`): Sets `scope = null` to inherit scope from parent project.
  * Submodule source filtering: Parent project automatically filters out child module source roots to prevent duplicate trees.
  * Supports AST extraction of `extends ... implements ...` supertypes, first-line Javadoc summaries, and both named inner classes (recursively) and anonymous inner classes (numbered `1`, `2`, `3`).
  * Alert scoping: Submodules search `moduleScope(m)`, while the parent aggregator searches `projectScope(p) - notScope(modules)` to prevent duplicating child alerts.
  * AST scan resilience: Catches stub-to-AST errors, appends `⚠️ [Javadoc unavailable]` inline, logs full warning with stack trace via `log.warn(..., t)`, and appends a summary callout block at the bottom of the structure section.

### B. Legacy Cleanup
- Safely deleted old 5 classes in `uno.anahata.asi.intellij.tools.project.context`: `ProjectContextProvider`, `ModuleContextProvider`, `ProjectStructureContextProvider`, `ProjectAlertsContextProvider`, and local `AbstractProjectContextProvider`.
- Updated `IntellijAgiConfig` and `IntellijIconProvider` to reference the clean renamed types.

---

## 5. Swing UI & Reactive Scoping (`anahata-asi-swing`)

### A. Specialized Granularity Panels (`uno.anahata.asi.swing.agi.project`)
- **`ProjectStructureScopePanel.java`**:
  * Responsive 3-column grid of the 9 granularity checkboxes.
  * Methods: `setScope(scope)`, `ProjectStructureScope getScope()`, `setEditable(boolean)`, `setOnScopeChanged(callback)`.
- **`ProjectsPanel.java`**:
  * Extends `AbstractToolkitRenderer<AbstractProjects>`.
  * Titled border: **`"Default Project Structure Scope"`**.
  * Embedded inside `ToolkitPanel`'s specialized UI container when `AbstractProjects` is selected.
  * Modifying any checkbox calls `toolkit.setDefaultScope(newScope)`.
- **`ProjectContextProviderPanel.java`**:
  * Extends `AbstractContextProviderRenderer<AbstractProjectContextProvider>`.
  * Titled border: **`"Project Structure Scope"`**.
  * Features the **Scope Mode Dropdown (`JComboBox`)**:
    * `"Inherit from Default Scope"`: Sets `provider.setScope(null)`, displays effective inherited scope, and grays out checkboxes.
    * `"Custom Scope Override"`: Sets `provider.setScope(newScope)`, enables checkboxes for local project tuning.
  * Embedded dynamically in `ContextProviderPanel` when an `AbstractProjectContextProvider` is selected.

### B. Reactive Presenters & Tree Nodes (`uno.anahata.asi.swing.agi.context`)
- **`ProjectContextProviderNode.java`**:
  * Dedicated node extending `ContextProviderNode`.
  * Binds an `EdtPropertyChangeListener` to `userObject` for `"projectStructureScope"`.
  * **Asynchronous Execution via `SwingTask`**: Dispatches token recalculation to a background worker thread. This eliminates the fatal `git4idea` assertion error on the EDT (`Assertion failed: Should not wait for built-in server on EDT`).
  * Cascades down to child modules whose `scope == null`.
  * Bubbles integer totals up through ancestor nodes to root (`bubbleUpTotals()`).
  * Notifies the tree model via `model.refreshNodeData(this)` and repaints on the EDT.
- **`ContextManagerNode.java`**:
  * Explicitly maps `AbstractProjectContextProvider` to `ProjectContextProviderNode`.
- **`ContextProviderUiRegistry.java`**:
  * Extensible singleton registry mapping `ContextProvider` types to specialized renderers (mirrors `ToolkitUiRegistry`).
- **`ContextPanel.java` Layout Fix**:
  * Removed outer scrollpane wrapper around `providerPanel` (`detailContainer.add(providerPanel, "provider")`).
  * Binds `providerPanel`'s width to the split pane's right half, allowing `RagMessageViewer` inside the tab to track viewport width.
  * Wide HTML Git tables and file paths now wrap cleanly without blowing out the layout or clipping text!
- **`RagMessageViewer.java`**:
  * Strongly typed to `RagMessage` and `RagMessagePanel`.
  * Dedicated transparent, borderless `JScrollPane` with smooth 16px vertical scrolling.
- **`ContextTableCellRenderer.java`**:
  * Overrides `setValue(Object)` using `NumberFormat.getIntegerInstance()` to format token counts with comma digit grouping (e.g. `3,794`, `127,759`).
  * Preserves numeric sorting via `Integer.class`.
- **`WrappingEditorPane.java` HiDPI Font Fix**:
  * Configured `putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE)`.
  * Bound to `UIManager.getFont("Label.font")`, eliminating double-scaling on Linux/HiDPI in IntelliJ.

---

## 6. Verification & Compilation Status

- **`anahata-asi-core`**: 0 Errors, 0 Alerts.
- **`anahata-asi-parent`**: 0 Errors, 0 Alerts.
- **`anahata-asi-nb`**: 0 Errors, 0 Alerts.
- **`anahata-asi-intellij`**: 0 Errors, 0 Alerts.
- **`anahata-asi-swing`**: 0 Errors, 0 Alerts.

*All modules compile cleanly. The environment is verified and ready for reload.*

---
*Força Barça! Universal project architecture established.*
