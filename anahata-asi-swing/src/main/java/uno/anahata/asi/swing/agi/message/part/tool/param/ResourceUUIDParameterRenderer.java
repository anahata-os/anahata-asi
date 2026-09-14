/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.Color;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;

/**
 * A specialized parameter renderer for an individual Resource UUID.
 * <p>
 * Extends {@link AbstractChipParameterRenderer} to display a compact green pill/chip
 * showing the Resource Name. It caches the name so it remains visible even if the
 * resource is later unloaded from the context window.
 * Supports full in-place editing, opening in IDE, and removal.
 * When placed inside a collection, it is composed by a list container
 * such as {@link WrapListParameterRenderer}.
 * </p>
 * 
 * @author anahata
 */
public class ResourceUUIDParameterRenderer extends AbstractChipParameterRenderer {

    /**
     * Constructs a new ResourceUUIDParameterRenderer.
     */
    public ResourceUUIDParameterRenderer() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>Resolves the cached or live resource name from ResourceManager.</p>
     */
    @Override
    protected String getDisplayName() {
        if (value == null || (value instanceof String s && s.isBlank())) {
            return "null";
        }
        String resourceUuid = value.toString();
        if (agiPanel != null && agiPanel.getAgi() != null && agiPanel.getAgi().getResourceManager() != null) {
            Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
            if (res != null) {
                return res.getName();
            }
        }
        return resourceUuid;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getTooltipText() {
        return value != null ? "UUID: " + value.toString() : null;
    }

    /**
     * {@inheritDoc}
     * <p>Opens the resource in the host IDE via ResourceUI.</p>
     */
    @Override
    protected void onOpen() {
        if (value != null && agiPanel != null && agiPanel.getAgi() != null) {
            String resourceUuid = value.toString();
            Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
            if (res != null && ResourceUiRegistry.getInstance().getResourceUI() != null) {
                ResourceUiRegistry.getInstance().getResourceUI().open(res, agiPanel);
            } else if (res == null) {
                JOptionPane.showMessageDialog(container, "The resource is no longer loaded in the context window.", "Resource Offline", JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getEphemeralFileName() {
        return "uuid.txt";
    }

    /**
     * {@inheritDoc}
     * <p>Theme-aware green background: light green in light themes, dark forest green in dark themes.</p>
     */
    @Override
    protected Color getPillBackground() {
        Color panelBg = UIManager.getColor("Panel.background");
        boolean isDark = panelBg != null && (panelBg.getRed() < 128 && panelBg.getGreen() < 128 && panelBg.getBlue() < 128);
        return isDark ? new Color(30, 55, 40) : new Color(230, 245, 235);
    }

    /**
     * {@inheritDoc}
     * <p>Theme-aware green border: light green border in light themes, subtle dark green border in dark themes.</p>
     */
    @Override
    protected Color getPillBorderColor() {
        Color panelBg = UIManager.getColor("Panel.background");
        boolean isDark = panelBg != null && (panelBg.getRed() < 128 && panelBg.getGreen() < 128 && panelBg.getBlue() < 128);
        return isDark ? new Color(45, 85, 60) : new Color(180, 220, 190);
    }
}
