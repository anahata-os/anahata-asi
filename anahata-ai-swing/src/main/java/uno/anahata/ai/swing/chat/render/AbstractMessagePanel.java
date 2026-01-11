/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

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
import javax.swing.border.Border;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.MattePainter;
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig;
import uno.anahata.ai.swing.icons.CopyIcon;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;
import uno.anahata.ai.swing.internal.SwingUtils;

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

    /** The parent chat panel. */
    protected final ChatPanel chatPanel;
    /** The message to render. */
    protected final T message;
    /** The chat configuration. */
    protected final SwingChatConfig chatConfig;

    /** Toggle button for pruning control. */
    private final PruningToggleButton pruningToggleButton;
    /** Button to copy the entire message content to the clipboard. */
    private final JButton copyButton;
    /** Button to remove the message from the chat history. */
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
     * @param chatPanel The parent chat panel.
     * @param message The message to render.
     */
    public AbstractMessagePanel(@NonNull ChatPanel chatPanel, @NonNull T message) {
        this.chatPanel = chatPanel;
        this.message = message;
        this.chatConfig = chatPanel.getChatConfig();

        // 1. Configure JXTitledPanel Header
        setTitleForeground(getHeaderForegroundColor());
        setTitleFont(new Font("SansSerif", Font.BOLD, 13));
        
        // Background Gradient via MattePainter
        MattePainter mp = new MattePainter(new GradientPaint(0, 0, getHeaderStartColor(), 1, 0, getHeaderEndColor()), true);
        setTitlePainter(mp);

        // 2. Initialize Header Buttons
        this.pruningToggleButton = new PruningToggleButton(message);
        
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
        rightButtonPanel.add(pruningToggleButton);
        rightButtonPanel.add(removeButton);
        setRightDecoration(rightButtonPanel);

        // 3. Setup Content Layout
        JXPanel mainContent = (JXPanel) getContentContainer();
        mainContent.setLayout(new BorderLayout());
        mainContent.setOpaque(true); // Make it opaque to show the background color
        mainContent.setBackground(getHeaderEndColor()); // Set the role-specific background
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
     */
    public final void render() {
        log.info("render() message #{}", message.getSequentialId());
        updateHeaderInfoText();
        renderContentParts();
        renderFooterInternal();
        revalidate();
        repaint();
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
        if (message.getChat() != null && message.getSequentialId() > 0) {
            return "<b>#" + message.getSequentialId() + "</b> ";
        }
        return "";
    }

    /**
     * Gets the sender information for the header.
     * @return The sender HTML.
     */
    protected String getHeaderSender() {
        return "<font color='#444444'><b>" + message.getFrom() + "</b></font> ";
    }

    /**
     * Gets the timestamp information for the header.
     * @return The timestamp HTML.
     */
    protected String getHeaderTimestamp() {
        return "<font color='#666666' size='3'>- " + TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(message.getTimestamp())) + "</font>";
    }

    /**
     * Gets the suffix for the header, typically metadata like depth.
     * @return The header suffix HTML.
     */
    protected String getHeaderSuffix() {
        return String.format(" <font color='#888888' size='3'><i>(Depth: %d)</i></font>", message.getDepth());
    }

    /**
     * Renders the content parts of the message using an incremental diffing mechanism.
     */
    protected void renderContentParts() {
        List<AbstractPart> currentParts = message.getParts();

        // 1. Identify and remove panels for parts no longer present
        List<AbstractPart> toRemove = cachedPartPanels.keySet().stream()
            .filter(part -> !currentParts.contains(part))
            .collect(Collectors.toList());
        
        for (AbstractPart removedPart : toRemove) {
            AbstractPartPanel panel = cachedPartPanels.remove(removedPart);
            if (panel != null) {
                partsContainer.remove(panel);
            }
        }

        // 2. Ensure all parts have panels and are in the correct order
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
                panel.render();
                
                if (componentIndex >= partsContainer.getComponentCount() || partsContainer.getComponent(componentIndex) != panel) {
                    partsContainer.add(panel, componentIndex);
                }
                componentIndex++;
            }
        }

        // 3. Clean up any trailing components
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
        if (part instanceof TextPart textPart) {
            return new TextPartPanel(chatPanel, textPart);
        } else if (part instanceof BlobPart blobPart) {
            return new BlobPartPanel(chatPanel, blobPart);
        }
        log.warn("No panel found for part type: {}", part.getClass().getName());
        return null;
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
