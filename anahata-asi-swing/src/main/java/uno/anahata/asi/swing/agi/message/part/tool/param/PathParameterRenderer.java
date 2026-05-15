/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * A specialized parameter renderer for absolute file paths.
 * <p>
 * It extends {@link UriParameterRenderer} to convert paths to URIs,
 * enabling the same rich chip UI and IDE navigation.
 * </p>
 * 
 * @author anahata
 */
public class PathParameterRenderer extends UriParameterRenderer {

    @Override
    public void updateContent(Object value) {
        if (value instanceof List<?> list) {
            List<String> uris = new ArrayList<>();
            for (Object item : list) {
                if (item != null) {
                    uris.add(new File(item.toString()).toURI().toString());
                }
            }
            super.updateContent(uris);
        } else if (value != null && !(value instanceof String s && s.isBlank())) {
            super.updateContent(new File(value.toString()).toURI().toString());
        } else {
            super.updateContent(null);
        }
    }
}
