/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Getter;
import uno.anahata.asi.agi.tool.Page;

/**
 * A specialized, token-efficient {@link Page} container for {@link MemberItem} instances.
 * <p>
 * Carries the enclosing type's FQN, language, and origin location once at the page level
 * rather than repeating redundant metadata on every individual member item.
 * </p>
 *
 * @author anahata
 */
@Getter
@Schema(description = "Paginated container of members belonging to a specific type across any JVM language.")
public class MemberPage extends Page<MemberItem> {

    /**
     * The fully-qualified name of the enclosing type whose members are listed.
     */
    @Schema(description = "The fully-qualified name of the enclosing type.")
    private final String typeFqn;

    /**
     * The source programming language of the enclosing type (e.g. 'JAVA', 'KOTLIN', 'GROOVY', 'SCALA').
     */
    @Schema(description = "The source language of the enclosing type.")
    private final String language;

    /**
     * The origin location of the enclosing type (e.g. 'Module: anahata-asi-core', 'SDK: 25').
     */
    @Schema(description = "The origin location of the enclosing type.")
    private final String location;

    /**
     * Constructs a new MemberPage.
     *
     * @param allItems   the complete list of candidate members.
     * @param startIndex the 0-based starting index for pagination.
     * @param pageSize   the maximum number of results to return per page.
     * @param typeFqn    the fully-qualified name of the declaring type.
     * @param language   the source programming language.
     * @param location   the origin location descriptor.
     */
    public MemberPage(List<MemberItem> allItems, int startIndex, int pageSize, String typeFqn, String language, String location) {
        super(allItems, startIndex, pageSize);
        this.typeFqn = typeFqn;
        this.language = language;
        this.location = location;
    }

    /**
     * {@inheritDoc}
     * <p>Formats the member page with enclosing type metadata and pagination summary.</p>
     */
    @Override
    public String toString() {
        return super.toString() + "\nEnclosing Type: " + typeFqn + " [" + language + "] (" + location + ")";
    }
}
