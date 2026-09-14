/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized parameter renderer for an individual URI string.
 * <p>
 * Extends {@link AbstractChipParameterRenderer} to display a compact pill/chip
 * with file name label, open action, and in-place high-fidelity editing.
 * When placed inside a collection, it is composed by a list container
 * such as {@link WrapListParameterRenderer} or {@link VBoxListParameterRenderer}.
 * </p>
 * 
 * @author anahata
 */
public class UriParameterRenderer extends AbstractChipParameterRenderer {

    /**
     * Constructs a new UriParameterRenderer.
     */
    public UriParameterRenderer() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Resolves the simple file name from the URI string.</p>
     */
    @Override
    protected String getDisplayName() {
        if (value == null) {
            return "null";
        }
        String str = value.toString();
        int lastSlash = str.lastIndexOf('/');
        if (lastSlash != -1 && lastSlash < str.length() - 1) {
            return str.substring(lastSlash + 1);
        }
        return str;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getTooltipText() {
        return value != null ? value.toString() : null;
    }

    /**
     * {@inheritDoc}
     * <p>Opens the URI using the registered ResourceUI strategy.</p>
     */
    @Override
    protected void onOpen() {
        if (value != null && ResourceUiRegistry.getInstance().getResourceUI() != null) {
            ResourceUiRegistry.getInstance().getResourceUI().openUri(value.toString());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getEphemeralFileName() {
        return "uri.txt";
    }
}
