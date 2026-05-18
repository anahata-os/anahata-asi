/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A structured representation of a Javadoc comment.
 * <p>
 * Used within CodeRefinementIntent to allow the LLM to provide structured
 * Javadoc alongside code modifications, bypassing RPM limits.
 * </p>
 */
@Data
@NoArgsConstructor
@Schema(description = "Structured Javadoc definition for a code refinement intent. WARNING: When used in an UPDATE intent, providing this object will completely replace the existing Javadoc. You MUST provide all @param, @return, and @throws fields if they should be preserved.")
public class JavadocIntent implements Serializable {

    /**
     * The main Javadoc description (Markdown or HTML). Supports inline tags like {@inheritDoc}, {@link}, {@code}.
     */
    @Schema(description = "The main Javadoc description.")
    private String description;
    /**
     * The list of author names associated with the @author tag.
     */
    @Schema(description = "List of author names.")
    private List<String> authors;
    /**
     * The list of versions associated with the @since tag.
     */
    @Schema(description = "List of since versions.")
    private List<String> since;
    /**
     * A map of parameter names to their descriptions for the @param tag.
     */
    @Schema(description = "Map of parameter names to their descriptions.")
    private Map<String, String> params;
    /**
     * The description of the return value for the @return tag.
     */
    @Schema(description = "The return value description.")
    private String returns;
    /**
     * A map of exception FQNs to their descriptions for the @throws tag.
     */
    @Schema(description = "Map of exception FQNs to their descriptions.")
    private Map<String, String> throwsList;
    /**
     * Optional list of additional block tags (e.g. ['version 1.0', 'see OtherClass']).
     */
    @Schema(description = "Optional list of additional block tags (e.g. ['version 1.0', 'see OtherClass']).")
    private List<String> tags;


    /**
     * Generates the multi-line Javadoc comment string from the current state.
     * @return a standard Javadoc block.
     */
    public String generateString() {
        StringBuilder sb = new StringBuilder("/**\n");
        if (description != null && !description.isBlank()) {
            for (String line : description.split("\n")) {
                sb.append(" * ").append(line).append("\n");
            }
        }
        if (authors != null) {
            for (String a : authors) {
                sb.append(" * @author ").append(a).append("\n");
            }
        }
        if (since != null) {
            for (String s : since) {
                sb.append(" * @since ").append(s).append("\n");
            }
        }
        if (params != null) {
            params.forEach((k, v) -> sb.append(" * @param ").append(k).append(" ").append(v).append("\n"));
        }
        if (returns != null && !returns.isBlank()) {
            sb.append(" * @return ").append(returns).append("\n");
        }
        if (throwsList != null) {
            throwsList.forEach((k, v) -> sb.append(" * @throws ").append(k).append(" ").append(v).append("\n"));
        }
        if (tags != null) {
            for (String tag : tags) {
                if (tag.startsWith("@")) {
                    sb.append(" * ").append(tag).append("\n");
                } else {
                    sb.append(" * @").append(tag).append("\n");
                }
            }
        }
        sb.append(" */");
        return sb.toString();
    }
}
