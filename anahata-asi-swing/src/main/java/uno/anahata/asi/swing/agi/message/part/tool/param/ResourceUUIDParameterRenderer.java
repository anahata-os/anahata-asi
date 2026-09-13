/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.icons.ExternalIcon;

/**
 * A specialized parameter renderer for an individual Resource UUID.
 * <p>
 * Renders a single compact chip showing the Resource Name. It caches 
 * the name so it remains visible even if the resource is later unloaded 
 * from the context window.
 * When placed inside a collection, it is composed by a list container
 * such as {@link WrapListParameterRenderer}.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class ResourceUUIDParameterRenderer extends AbstractParameterRenderer<Object> {

    /** The chip container representing this single resource. */
    private final JPanel container = new JPanel(new BorderLayout(10, 0));

    /**
     * Constructs a new ResourceUUIDParameterRenderer.
     */
    public ResourceUUIDParameterRenderer() {
        container.setOpaque(true);
        container.setBackground(new java.awt.Color(230, 245, 235)); // Slightly greenish for resources
        container.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new java.awt.Color(180, 220, 190), 1, true),
                BorderFactory.createEmptyBorder(4, 8, 4, 8)
        ));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateContent(Object value) {
        this.value = value;
    }

    /**
     * {@inheritDoc}
     * <p>Renders a single chip for the bound resource UUID.</p>
     */
    @Override
    public boolean render() {
        container.removeAll();

        if (value == null || (value instanceof String s && s.isBlank())) {
            container.add(new JLabel("null"), BorderLayout.CENTER);
            return true;
        }

        String resourceUuid = value.toString();
        String cachedDisplayName = resourceUuid;
        if (agiPanel != null && agiPanel.getAgi() != null && agiPanel.getAgi().getResourceManager() != null) {
            Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
            if (res != null) {
                cachedDisplayName = res.getName();
            }
        }

        JLabel label = new JLabel(cachedDisplayName);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setToolTipText("UUID: " + resourceUuid);
        container.add(label, BorderLayout.CENTER);

        JButton openBtn = new JButton(new ExternalIcon(14));
        openBtn.setToolTipText("Open Resource in IDE");
        openBtn.setMargin(new Insets(0, 2, 0, 2));
        openBtn.setContentAreaFilled(false);
        openBtn.setBorderPainted(false);
        openBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

        openBtn.addActionListener(e -> {
            if (agiPanel != null && agiPanel.getAgi() != null) {
                Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
                if (res != null && ResourceUiRegistry.getInstance().getResourceUI() != null) {
                    ResourceUiRegistry.getInstance().getResourceUI().open(res, agiPanel);
                } else if (res == null) {
                    JOptionPane.showMessageDialog(container, "The resource is no longer loaded in the context window.", "Resource Offline", JOptionPane.WARNING_MESSAGE);
                }
            }
        });

        container.add(openBtn, BorderLayout.EAST);
        container.revalidate();
        container.repaint();
        return true;
    }
}
