/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import java.util.ArrayList;
import java.util.List;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import uno.anahata.asi.ide.tools.hints.HintInfo;

/**
 * IntelliJ-specific diagnostic DTO extending {@link HintInfo} with available quick-fix actions.
 * <p>
 * Captures intention action names associated with an IntelliJ {@code HighlightInfo}
 * and formats them directly into the Markdown representation, allowing AI models to execute
 * single-shot fixes via {@link IntellijHints#applyHint(String, int, String)}.
 * </p>
 *
 * @author anahata
 */
@Getter
@Setter
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class IntellijHintInfo extends HintInfo {

    /**
     * The list of human-readable intention action names (quick fixes) available for this diagnostic.
     */
    private List<String> quickFixes = new ArrayList<>();

    /**
     * Constructs a new IntellijHintInfo with full diagnostic metadata and quick-fix actions.
     *
     * @param filePath The absolute canonical path of the file containing the hint.
     * @param description A human-readable description or diagnostic message.
     * @param severity The localized severity string reported by IntelliJ.
     * @param line The 1-based line number where the diagnostic starts.
     * @param column The 1-based column number where the diagnostic starts.
     * @param id The inspection tool ID or short name.
     * @param quickFixes The list of available quick-fix action names.
     */
    public IntellijHintInfo(String filePath, String description, String severity, int line, int column, String id, List<String> quickFixes) {
        super(filePath, description, severity, line, column, id);
        if (quickFixes != null) {
            this.quickFixes = quickFixes;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Appends available IntelliJ quick-fix action names
     * to the standard Markdown hint line in the format {@code [Fixes: action1, action2]}.
     * </p>
     */
    @Override
    public String toMarkdown() {
        StringBuilder sb = new StringBuilder(super.toMarkdown());
        if (quickFixes != null && !quickFixes.isEmpty()) {
            sb.append(" [Fixes: ").append(String.join(", ", quickFixes)).append("]");
        }
        return sb.toString();
    }
}
