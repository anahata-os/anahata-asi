/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.util.List;
import uno.anahata.asi.toolkit.files.FullTextFileUpdate;
import uno.anahata.asi.toolkit.files.LineComment;

/**
 * A rich renderer for {@link FullTextFileUpdate} tool parameters.
 * 
 * @author anahata
 */
public class FullTextFileUpdateRenderer extends AbstractTextFileWriteRenderer<FullTextFileUpdate> {

    @Override
    protected String calculateProposedContent(String currentContent) {
        return update.getNewContent();
    }

    @Override
    protected List<LineComment> getLineComments(String currentContent) {
        return update.getLineComments();
    }

    @Override
    protected FullTextFileUpdate createUpdatedDto(String newContent) {
        return new FullTextFileUpdate(
                update.getPath(),
                update.getLastModified(),
                newContent,
                update.getLineComments()
        );
    }
}
