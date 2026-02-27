/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.MattePainter;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.PinnedIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A base class for rendering {@link AbstractMessage} instances in a collapsible,
 * diff-based UI component. It leverages {@link JXTitledPanel} to provide a
 * styled header with message metadata and pruning controls, and a content area 
 * that dynamically renders {@link AbstractPart}s using a diffing mechanism.
 *
 * @author anahata
 * @param <T> The concrete type of AbstractMessage that this panel renders.
 */
@Slf4j
public abstract class AbstractMessagePanel<T extends AbstractMessage> extends JXTitledPanel {

    /** The parent agi panel. */
    protected final AgiPanel agiPanel;
    /** The message to render. */
    protected final T message;
    /** The agi configuration. */
    protected final SwingAgiConfig agiConfig;

    /** Master toggle button for pinning all parts in the interaction. */
    private final JToggleButton pinButton;
    /** Button to copy the entire message content to the clipboard. */
    private final JButton copyButton;
    /** Button to remove the message from the agi history. */
    private final JButton removeButton;

    /** Container for message parts. */
    private final JXPanel partsContainer;
    /** Container for message-level metadata and actions. */
    protected final JPanel footerContainer;
    
    /** Cache of part panels to support incremental updates. */
    private final Map<AbstractPart, AbstractPartPanel> cachedPartPanels = new HashMap<>();

    /**
     * Constructs a new AbstractMessagePanel.
     *
     * @param agiPanel The parent agi panel.
     * @param message The message to render.
     */
    public AbstractMessagePanel(@NonNull AgiPanel agiPanel, @NonNull T message) {
        this.agiPanel = agiPanel;
        this.message = message;
        this.agiConfig = agiPanel.getAgiConfig();

        // 1. Configure JXTitledPanel Header
        setTitleForeground(getHeaderForegroundColor());
        setTitleFont(new Font("SansSerif", Font.BOLD, 13));
        
        // 2. Initialize Header Buttons
        this.pinButton = new JToggleButton(new PinnedIcon(16));
        this.pinButton.setToolTipText("Pin Interaction (Keep all parts in context)");
        this.pinButton.setMargin(new java.awt.Insets(0, 4, 0, 4));
        this.pinButton.addActionListener(e -> {
            if (pinButton.isSelected()) {
                message.pinAllParts();
            } else {
                message.setAutoAllParts();
            }
        });
        
        this.copyButton = new JButton(new CopyIcon(16));
        this.copyButton.setToolTipText("Copy Message Content");
        this.copyButton.setMargin(new java.awt.Insets(0, 4, 0, 4));
        this.copyButton.addActionListener(e -> SwingUtils.copyToClipboard(message.asText(false)));

        this.removeButton = new JButton(new DeleteIcon(16));
        this.removeButton.setToolTipText("Remove Message");
        this.removeButton.setMargin(new java.awt.Insets(0, 4, 0, 4));
        this.removeButton.addActionListener(e -> message.remove());

        // Copy button on the left
        setLeftDecoration(copyButton);

        // Pruning and Remove buttons on the right
        JPanel rightButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        rightButtonPanel.setOpaque(false);
        rightButtonPanel.add(pinButton);
        rightButtonPanel.add(removeButton);
        setRightDecoration(rightButtonPanel);

        // 3. Setup Content Layout
        JXPanel mainContent = (JXPanel) getContentContainer();
        mainContent.setLayout(new BorderLayout());
        mainContent.setOpaque(true); // Make it opaque to show the background color
        mainContent.setBorder(BorderFactory.createEmptyBorder(5, 12, 10, 12));

        partsContainer = new JXPanel();
        partsContainer.setLayout(new BoxLayout(partsContainer, BoxLayout.Y_AXIS));
        partsContainer.setOpaque(false);
        mainContent.add(partsContainer, BorderLayout.CENTER);

        footerContainer = new JPanel();
        footerContainer.setLayout(new BoxLayout(footerContainer, BoxLayout.Y_AXIS));
        footerContainer.setOpaque(false);
        mainContent.add(footerContainer, BorderLayout.SOUTH);
        
        setOpaque(false);
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEmptyBorder(5, 0, 5, 0),
                getMessageBorder()
        ));

        // 4. Expand/Collapse Logic on Header Click
        if (getComponentCount() > 0) {
            getComponent(0).addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    toggleExpanded();
                }
            });
        }

        // Declarative, thread-safe binding to message properties
        new EdtPropertyChangeListener(this, message, "pruned", evt -> render());
        new EdtPropertyChangeListener(this, message, "parts", evt -> render());
        // Global listener for "show pruned" toggle to refresh the message structure.
        new EdtPropertyChangeListener(this, agiConfig, "showPruned", evt -> render());
    }

    /**
     * Toggles the visibility of the content container.
     */
    private void toggleExpanded() {
        getContentContainer().setVisible(!getContentContainer().isVisible());
        revalidate();
        repaint();
    }

    /**
     * Called when the component is added to its parent. Performs the initial render.
     */
    @Override
    public void addNotify() {
        super.addNotify();
        render();
    }

    /**
     * Renders or updates the entire message panel, including its header, content parts, and footer.
     * <p>
     * This method is now strictly structural for parts: it adds or removes part panels 
     * but does NOT call render() on them.
     * </p>
     */
    public final void render() {
        log.info("Updating message structure for #{}", message.getSequentialId());
        updateHeaderInfoText();
        updateHeaderButtons();
        updateBackgroundColors();
        renderContentParts();
        renderFooterInternal();
        updateVisibility();
        revalidate();
        repaint();
    }

    /**
     * Refreshes the transient metadata in the header (e.g., Depth) and propagates
     * the refresh to all child parts. This ensures that as the conversation
     * grows, all labels reflect the current distance from the head.
     */
    public void refreshMetadata() {
        updateHeaderInfoText();
        // If we are physically filtering, we must re-evaluate the parts structure 
        // because the remaining depth change might have caused a part to "expire" 
        // into the filtered-out state.
        if (!agiConfig.isShowPruned()) {
            renderContentParts();
        }
        cachedPartPanels.values().forEach(AbstractPartPanel::refreshMetadata);
    }

    /**
     * Updates the background colors of the header and content area based on the pruned state.
     */
    protected void updateBackgroundColors() {
        boolean isEffectivelyPruned = message.isEffectivelyPruned();
        Color start = getHeaderStartColor();
        Color end = getHeaderEndColor();
        
        if (isEffectivelyPruned) {
            // Distinct 'Ghosted' style for pruned messages
            start = new Color(235, 235, 235);
            end = new Color(242, 242, 242);
            setTitleForeground(new Color(120, 120, 120));
        } else {
            setTitleForeground(getHeaderForegroundColor());
        }
        
        MattePainter mp = new MattePainter(new GradientPaint(0, 0, start, 1, 0, end), true);
        setTitlePainter(mp);
        getContentContainer().setBackground(end);
    }

    /**
     * Updates the visibility of header buttons based on the render flags.
     */
    protected void updateHeaderButtons() {
        pinButton.setVisible(isRenderPruneButtons());
        removeButton.setVisible(isRenderRemoveButtons());
        
        // The master pin is selected ONLY if everything is pinned
        pinButton.setSelected(message.isAllPinned());
    }

    /**
     * Updates the text displayed in the header's title.
     * This method uses a template pattern, delegating to smaller methods for specific parts of the header.
     */
    protected void updateHeaderInfoText() {
        StringBuilder sb = new StringBuilder("<html>");
        sb.append(getHeaderPrefix());
        sb.append(getHeaderSender());
        sb.append(getHeaderTimestamp());
        sb.append(getHeaderSuffix());
        sb.append("</html>");
        
        setTitle(sb.toString());
    }

    /**
     * Gets the prefix for the header, typically the sequential ID.
     * @return The header prefix HTML.
     */
    protected String getHeaderPrefix() {
        if (message.getAgi() != null && message.getSequentialId() > 0) {
            return "<b>#" + message.getSequentialId() + "</b> ";
        }
        return "";
    }

    /**
     * Gets the sender information for the header.
     * @return The sender HTML.
     */
    protected String getHeaderSender() {
        String color = message.isEffectivelyPruned() ? "#888888" : "#444444";
        return "<font color='" + color + "'><b>" + message.getFrom() + "</b></font> ";
    }

    /**
     * Gets the timestamp information for the header.
     * @return The timestamp HTML.
     */
    protected String getHeaderTimestamp() {
        return "<font color='#999999' size='3'>- " + TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(message.getTimestamp())) + "</font>";
    }

    /**
     * Gets the suffix for the header, typically metadata like depth.
     * @return The header suffix HTML.
     */
    protected String getHeaderSuffix() {
        return String.format(" <font color='#aaaaaa' size='3'><i>(Depth: %d)</i></font>", message.getDepth());
    }

    /**
     * Renders the content parts of the message using an incremental diffing mechanism.
     */
    protected void renderContentParts() {
        List<AbstractPart> allParts = message.getParts();
        
        // 1. Identify and remove panels for parts that are PERMANENTLY deleted from the model.
        // We do NOT remove from cache if they are just filtered out.
        List<AbstractPart> deletedFromModel = cachedPartPanels.keySet().stream()
            .filter(part -> !allParts.contains(part))
            .collect(Collectors.toList());
        
        for (AbstractPart removedPart : deletedFromModel) {
            AbstractPartPanel panel = cachedPartPanels.remove(removedPart);
            if (panel != null) {
                partsContainer.remove(panel);
            }
        }

        // 2. Resolve the filtered list for display.
        final List<AbstractPart> currentParts;
        if (!agiConfig.isShowPruned()) {
            currentParts = allParts.stream()
                    .filter(p -> !p.isEffectivelyPruned())
                    .collect(Collectors.toList());
        } else {
            currentParts = allParts;
        }

        // 3. Ensure all visible parts have panels and are in the correct order in the container.
        // This structural update is very cheap in Swing compared to content rendering.
        int componentIndex = 0;
        for (int i = 0; i < currentParts.size(); i++) {
            AbstractPart part = currentParts.get(i);
            AbstractPartPanel panel = cachedPartPanels.get(part);
            
            if (panel == null) {
                panel = createPartPanel(part);
                if (panel != null) {
                    cachedPartPanels.put(part, panel);
                }
            }

            if (panel != null) {
                if (componentIndex >= partsContainer.getComponentCount() || partsContainer.getComponent(componentIndex) != panel) {
                    partsContainer.add(panel, componentIndex);
                }
                componentIndex++;
            }
        }

        // 4. Clean up any trailing components (panels that were visible but are now filtered out).
        while (partsContainer.getComponentCount() > componentIndex) {
            partsContainer.remove(partsContainer.getComponentCount() - 1);
        }
    }

    private void renderFooterInternal() {
        renderFooter();
    }

    /**
     * Template method for subclasses to add components to the message footer.
     * Subclasses should use the {@code footerContainer} field directly.
     */
    protected void renderFooter() {
        // Default implementation does nothing
    }

    /**
     * Factory method to create a specific part panel for a given part.
     *
     * @param part The part to create a panel for.
     * @return The created part panel, or null if no panel is available for the part type.
     */
    protected AbstractPartPanel createPartPanel(AbstractPart part) {
        return PartPanelFactory.createPartPanel(agiPanel, part);
    }

    /**
     * Updates the visibility of the entire message panel based on the pruned state.
     */
    protected void updateVisibility() {
        boolean isEffectivelyPruned = message.isEffectivelyPruned();
        setVisible(!isEffectivelyPruned || agiConfig.isShowPruned());
    }

    /**
     * If true, the pruning toggle button is rendered in the header.
     * @return true by default.
     */
    public boolean isRenderPruneButtons() {
        return true;
    }

    /**
     * If true, the remove button is rendered in the header.
     * @return true by default.
     */
    public boolean isRenderRemoveButtons() {
        return true;
    }

    /**
     * Gets the start color for the header gradient.
     * @return The start color.
     */
    protected abstract Color getHeaderStartColor();
    /**
     * Gets the end color for the header gradient.
     * @return The end color.
     */
    protected abstract Color getHeaderEndColor();
    /**
     * Gets the foreground color for the header text.
     * @return The foreground color.
     */
    protected abstract Color getHeaderForegroundColor();
    /**
     * Gets the border for the message panel.
     * @return The border.
     */
    protected abstract Border getMessageBorder();
}
