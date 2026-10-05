/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.tools.hints;

import java.util.List;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Universal abstract base class for host IDE code inspection and hint toolkits.
 * <p>
 * Defines the core contract for querying live file-level diagnostics (warnings,
 * errors, verify alerts, and suggestions) across different host IDE platforms
 * (NetBeans, IntelliJ IDEA, and Eclipse).
 * </p>
 * <p>
 * In accordance with Anahata IDE abstraction principles, this base class intentionally
 * omits {@code @AgiTool} annotations so that concrete IDE implementations retain full
 * ownership of tool descriptions and parameter prompts reflecting the native mental model
 * and idioms of that specific host IDE.
 * </p>
 *
 * @author anahata
 */
public abstract class AbstractHints extends AnahataToolkit {

    /**
     * Constructs a new AbstractHints toolkit instance.
     */
    public AbstractHints() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Supplies universal instructions for IDE inspection toolkits,
     * clarifying that live inspection hints and quick-fix actions are evaluated JIT after tool
     * execution and right before the turn starts in the resource footer of in-context files,
     * eliminating the need to call {@code getFileHints} on loaded resources.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return List.of(
                "- **Live Inspection Hints in Resource Footer**: For any text resource loaded in context, live code inspection diagnostics, compiler warnings, errors, and available quick-fix action names are evaluated JIT after tool execution and right before the turn starts, displayed in the resource footer, regardless of whether the resource has a LIVE or SNAPSHOT refresh policy.",
                "- **Quick-Fix Execution Badges**: Quick fixes in `[Fixes: ...]` are annotated with execution badges: `(⚡)` indicates a headless fix that can be executed programmatically via `applyHint` in a single turn; `(👤)` indicates an interactive fix (e.g. dialogs, chooser popups, dictionary selectors) requiring user interaction in the IDE UI.",
                "- **Avoid Redundant getFileHints**: DO NOT invoke `getFileHints` on resources that are already in context, as their active diagnostics and available quick-fix actions are already visible in their resource footer."
        );
    }

    /**
     * Queries all active inspection warnings, errors, and hints for the specified file.
     *
     * @param filePath The absolute filesystem path of the file to inspect.
     * @return A list of {@link HintInfo} diagnostics found in the file, or an empty list if none.
     * @throws Exception If inspection fails or the file cannot be analyzed.
     */
    public abstract List<HintInfo> getFileHints(String filePath) throws Exception;
}
