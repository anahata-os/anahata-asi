/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.components.WrapLayout;
import uno.anahata.asi.swing.icons.ExternalIcon;
import javax.swing.JOptionPane;

/**
 * A specialized parameter renderer for Resource UUIDs.
 * <p>
 * This renderer provides a sleek chip showing the Resource Name. It caches 
 * the name so it remains visible even if the resource is later unloaded 
 * from the context window.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ResourceUUIDParameterRenderer implements ParameterRenderer<Object> {

    /** The parent agi panel providing context. */
    private AgiPanel agiPanel;
    /** The current object value. */
    private Object value;
    /** The main UI container. */
    private final JPanel container = new JPanel(new WrapLayout(FlowLayout.LEFT, 5, 5));

    /**
     * Constructs a new ResourceUUIDParameterRenderer.
     */
    public ResourceUUIDParameterRenderer() {
        container.setOpaque(false);
    }

    /** {@inheritDoc} */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        this.agiPanel = agiPanel;
        updateContent(value);
    }

    /** {@inheritDoc} */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** {@inheritDoc} */
    @Override
    public void updateContent(Object value) {
        this.value = value;
    }

    /** {@inheritDoc} */
    @Override
    public boolean render() {
        container.removeAll();

        if (value == null || (value instanceof String s && s.isBlank())) {
            container.add(new JLabel("null"));
            return true;
        }

        if (value instanceof java.util.List<?> list) {
            for (Object item : list) {
                if (item != null) {
                    container.add(createChip(item.toString()));
                }
            }
        } else {
            container.add(createChip(value.toString()));
        }

        container.revalidate();
        container.repaint();
        return true;
    }

    /**
     * Helper to create a visual JPanel chip representing the resource.
     * @param resourceUuid The unique identifier of the resource.
     * @return a styled chip component with open action button.
     */
    private JPanel createChip(String resourceUuid) {
        JPanel chip = new JPanel(new BorderLayout(10, 0));
        chip.setOpaque(true);
        chip.setBackground(new java.awt.Color(230, 245, 235)); // Slightly greenish for resources
        chip.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new java.awt.Color(180, 220, 190), 1, true),
                BorderFactory.createEmptyBorder(4, 8, 4, 8)
        ));

        String cachedDisplayName = resourceUuid; // Default
        if (resourceUuid != null && !resourceUuid.isBlank()) {
            Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
            if (res != null) {
                cachedDisplayName = res.getName();
            }
        }

        JLabel label = new JLabel(cachedDisplayName);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setToolTipText("UUID: " + resourceUuid);
        chip.add(label, BorderLayout.CENTER);

        JButton openBtn = new JButton(new ExternalIcon(14));
        openBtn.setToolTipText("Open Resource");
        openBtn.setMargin(new java.awt.Insets(0, 2, 0, 2));
        openBtn.setContentAreaFilled(false);
        openBtn.setBorderPainted(false);
        openBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        
        openBtn.addActionListener(e -> {
            Resource res = agiPanel.getAgi().getResourceManager().get(resourceUuid);
            if (res != null && ResourceUiRegistry.getInstance().getResourceUI() != null) {
                ResourceUiRegistry.getInstance().getResourceUI().open(res, agiPanel);
            } else if (res == null) {
                JOptionPane.showMessageDialog(container, "The resource is no longer loaded in the context window.", "Resource Offline", JOptionPane.WARNING_MESSAGE);
            }
        });
        
        chip.add(openBtn, BorderLayout.EAST);
        return chip;
    }
}