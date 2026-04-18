/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.List;
import lombok.Getter;
import uno.anahata.asi.agi.tool.Page;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A specialized Page for JavaMembers that holds a single URL representing 
 * the source/class file for all members in the page.
 * This is a token-saving optimization to avoid duplicating the URL for every member.
 */
@Getter
public class JavaMemberPage extends Page<JavaMember> {
    
    /** The URL pointing to the source or class file where all these members are defined. */
    @Schema(description = "The URL pointing to the source or class file where all these members are defined.")
    private final URL urlOfAllMembers;

    /**
     * Constructs a new JavaMemberPage.
     * @param allItems the complete list of members.
     * @param startIndex the starting index.
     * @param pageSize the page size.
     * @param url the URL for all members in this page.
     */
    public JavaMemberPage(List<JavaMember> allItems, int startIndex, int pageSize, URL url) {
        super(allItems, startIndex, pageSize);
        this.urlOfAllMembers = url;
    }
    
    @Override
    public String toString() {
        return super.toString() + "\nSource URL for all members: " + urlOfAllMembers;
    }
}
