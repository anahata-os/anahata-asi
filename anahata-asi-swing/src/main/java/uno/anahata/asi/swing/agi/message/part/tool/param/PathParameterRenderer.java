/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.io.File;

/**
 * A specialized parameter renderer for an individual file path string.
 * <p>
 * It extends {@link UriParameterRenderer} to convert a file path to a {@code file:} URI,
 * enabling the same compact chip UI and host IDE navigation.
 * When placed inside a collection, it is composed by a list container
 * such as {@link WrapListParameterRenderer}.
 * </p>
 * 
 * @author anahata
 */
public class PathParameterRenderer extends UriParameterRenderer {

    /**
     * {@inheritDoc}
     * <p>Converts a single file path string to a file: URI string.</p>
     */
    @Override
    public void updateContent(Object value) {
        if (value != null && !(value instanceof String s && s.isBlank())) {
            super.updateContent(new File(value.toString()).toURI().toString());
        } else {
            super.updateContent(null);
        }
    }
}
