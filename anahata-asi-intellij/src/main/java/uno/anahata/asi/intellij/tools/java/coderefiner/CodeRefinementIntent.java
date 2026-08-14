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
}
