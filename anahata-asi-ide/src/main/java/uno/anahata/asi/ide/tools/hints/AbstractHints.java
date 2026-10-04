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
     * Queries all active inspection warnings, errors, and hints for the specified file.
     *
     * @param filePath The absolute filesystem path of the file to inspect.
     * @return A list of {@link HintInfo} diagnostics found in the file, or an empty list if none.
     * @throws Exception If inspection fails or the file cannot be analyzed.
     */
    public abstract List<HintInfo> getFileHints(String filePath) throws Exception;
}
