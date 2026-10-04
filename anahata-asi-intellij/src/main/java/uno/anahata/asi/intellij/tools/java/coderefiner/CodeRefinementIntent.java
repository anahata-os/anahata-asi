/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * A single structural, member-level modification to one Java file, applied by
 * {@code BatchCodeRefiner} against the live PSI tree.
 * <p>
 * This is the IntelliJ counterpart of the NetBeans V4 {@code CodeRefinementIntent}. Each
 * intent targets a class (for {@link Type#INSERT}) or an existing member by canonical FQN
 * (for {@link Type#UPDATE}/{@link Type#DELETE}/{@link Type#MOVE}); the member source is
 * supplied verbatim in {@link #declaration} and parsed by the IntelliJ PSI element factory.
 * </p>
 *
 * @author anahata
 */
@Data
public class CodeRefinementIntent {

    /**
     * The kind of structural modification performed by an intent.
     */
    public enum Type {

        /** Add a new member (method/field/inner class/initializer) to a target class. */
        INSERT,

        /** Replace an existing member (matched by FQN) with a new declaration. */
        UPDATE,

        /** Remove an existing member (matched by FQN). */
        DELETE,

        /** Relocate an existing member within its class relative to an anchor. */
        MOVE
    }

    /**
     * The kind of modification to perform.
     */
    @Schema(description = "The kind of modification: INSERT, UPDATE, DELETE or MOVE.")
    private Type type;

    /**
     * The canonical FQN of the class to insert into (for INSERT).
     */
    @Schema(description = "For INSERT: the canonical FQN of the target class to add the member to.")
    private String classFqn;

    /**
     * The canonical FQN of the member to update/delete/move (method or field).
     */
    @Schema(description = "For UPDATE/DELETE/MOVE: the canonical FQN of the target member, e.g. 'com.foo.Bar.doIt(int)' or 'com.foo.Bar.count'.")
    private String memberFqn;

    /**
     * The verbatim Java source of the member (for INSERT/UPDATE), including any Javadoc,
     * annotations and modifiers.
     */
    @Schema(description = "For INSERT/UPDATE: the full Java source of the member (Javadoc + annotations + modifiers + body).")
    private String declaration;

    /**
     * The placement of the member relative to the class body or anchor (INSERT/MOVE).
     */
    @Schema(description = "For INSERT/MOVE: placement relative to the class body or anchor member.")
    private RelativePosition position;

    /**
     * The simple name of the anchor member for BEFORE/AFTER placement (INSERT/MOVE).
     */
    @Schema(description = "For INSERT/MOVE with BEFORE/AFTER: the simple name of the anchor member.")
    private String anchorMemberName;

    /**
     * A short human rationale for this change (surfaced to the user, not applied).
     */
    @Schema(description = "A short human-readable rationale for this change.")
    private String reason;

    /**
     * Generates a rich HTML representation of this intent with colored badges for UI rendering.
     *
     * @return an HTML-formatted string.
     */
    public String getHtmlDisplay() {
        String color = switch (type) {
            case INSERT -> "#4CAF50";
            case UPDATE -> "#2196F3";
            case DELETE -> "#F44336";
            case MOVE -> "#FF9800";
        };
        String icon = switch (type) {
            case INSERT -> "[+]";
            case UPDATE -> "[*]";
            case DELETE -> "[-]";
            case MOVE -> "[M]";
        };

        String targetName = (memberFqn != null) ? getSimpleName(memberFqn) : "New Member";
        if (type == Type.INSERT && declaration != null) {
            targetName = getSimpleNameFromDeclaration(declaration);
        }

        StringBuilder sb = new StringBuilder("<font color='").append(color).append("'>").append(icon).append("</font> ");
        sb.append("<b>").append(type.toString().toUpperCase()).append("</b> <code>").append(targetName).append("</code>");

        if (position != null) {
            sb.append(" ").append(position);
            if (anchorMemberName != null) {
                sb.append(" ").append(getSimpleName(anchorMemberName));
            }
        }

        if (reason != null && !reason.isBlank()) {
            sb.append(" <i style='color: #888888;'>(").append(reason).append(")</i>");
        }

        return sb.toString();
    }

    /**
     * Extracts the simple name from a fully qualified name.
     *
     * @param fqn the fully qualified name.
     * @return the simple name.
     */
    private String getSimpleName(String fqn) {
        if (fqn == null || fqn.isBlank()) {
            return "Unknown";
        }
        int paren = fqn.indexOf('(');
        String namePart = (paren == -1) ? fqn : fqn.substring(0, paren);
        int lastDot = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return (lastDot == -1) ? namePart : namePart.substring(lastDot + 1);
    }

    /**
     * Parses a member declaration string to extract its simple name.
     *
     * @param decl the declaration string.
     * @return the simple name.
     */
    private String getSimpleNameFromDeclaration(String decl) {
        if (decl == null) {
            return "Unknown";
        }
        String clean = decl.trim();
        while (clean.startsWith("@")) {
            int space = clean.indexOf(' ');
            if (space == -1) {
                break;
            }
            clean = clean.substring(space).trim();
        }
        int paren = clean.indexOf('(');
        int end = (paren != -1) ? paren : (clean.endsWith(";") ? clean.length() - 1 : clean.length());
        int start = clean.lastIndexOf(' ', end - 1);
        if (start == -1) {
            start = 0;
        }
        String name = clean.substring(start + 1, end).trim();
        return (paren != -1) ? name + "()" : name;
    }
}
