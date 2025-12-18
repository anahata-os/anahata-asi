/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.Painter;
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;

/**
 * A base class for rendering {@link AbstractMessage} instances in a collapsible,
 * diff-based UI component. It extends {@link JXTitledPanel} to provide a
 * header with message metadata and pruning controls, and a content area that
 * dynamically renders {@link AbstractPart}s using a diffing mechanism.
 *
 * @author pablo
 * @param <T> The concrete type of AbstractMessage that this panel renders.
 */
@Slf4j
@Getter
public abstract class AbstractMessagePanel<T extends AbstractMessage> extends JXTitledPanel {

    /**
     * The parent chat panel.
     */
    protected final ChatPanel chatPanel;
    /**
     * The message to render.
     */
    protected final T message;
    /**
     * The chat configuration.
     */
    protected final SwingChatConfig chatConfig;

    /**
     * Custom header for rich display.
     */
    private final GradientHeaderPanel headerPanel;
    /**
     * Toggle button for pruning control.
     */
    private final PruningToggleButton pruningToggleButton;
    /**
     * Button to remove the message from the chat history.
     */
    private final JButton removeButton;

    /**
     * Content area for the message's parts.
     */
    private final JPanel contentPartsPanel;
    /**
     * Cache of part panels to support incremental updates.
     */
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

        // Configure JXTitledPanel
        setTitle(message.getRole().name());
        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        // Custom header for rich display
        setAlpha(0.9f);
        setPaintBorderInsets(true);

        // Initialize GradientHeaderPanel
        this.headerPanel = new GradientHeaderPanel(getHeaderStartColor(), getHeaderEndColor(), getHeaderForegroundColor());
        this.pruningToggleButton = new PruningToggleButton(message);
        
        // Initialize Remove Button
        this.removeButton = new JButton(IconUtils.getIcon("delete.png"));
        this.removeButton.setToolTipText("Remove Message");
        this.removeButton.setMargin(new java.awt.Insets(0, 4, 0, 4));
        this.removeButton.addActionListener(e -> message.remove());

        JPanel headerContent = new JPanel(new BorderLayout());
        headerContent.setOpaque(false);
        headerContent.add(headerPanel, BorderLayout.CENTER);
        
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        buttonPanel.setOpaque(false);
        buttonPanel.add(pruningToggleButton);
        buttonPanel.add(removeButton);
        headerContent.add(buttonPanel, BorderLayout.EAST);

        // FIX: Use Painter<Object> to avoid ClassCastException from SwingX UI delegate
        setTitlePainter(new Painter<Object>() {
            @Override
            public void paint(Graphics2D g, Object object, int width, int height) {
                headerContent.setSize(width, height);
                headerContent.paint(g);
            }
        });

        // Initialize content panel
        contentPartsPanel = new JPanel();
        contentPartsPanel.setLayout(new BoxLayout(contentPartsPanel, BoxLayout.Y_AXIS));
        contentPartsPanel.setOpaque(false);
        contentPartsPanel.setBorder(BorderFactory.createEmptyBorder(10, 12, 10, 12));
        add(contentPartsPanel, BorderLayout.CENTER);

        // Declarative, thread-safe binding to message properties
        new EdtPropertyChangeListener(this, message, "pruned", evt -> render());
        new EdtPropertyChangeListener(this, message, "parts", evt -> render());
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
     * Renders or updates the entire message panel, including its header and content parts.
     */
    public final void render() {
        updateHeaderInfoText();
        renderContentParts();
        revalidate();
        repaint();
    }

    /**
     * Updates the text displayed in the header's info label.
     */
    protected void updateHeaderInfoText() {
        String headerText = String.format("<html><b>%S</b> [#%d]", message.getRole().name(), message.getSequentialId());
        headerText += " <font color='#666666'>- " + TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(message.getTimestamp())) + "</font>";
        headerText += String.format(" <font color='#888888'><i>(Tokens: %d)</i></font>", message.getTokenCount());
        headerText += String.format(" <font color='#888888'><i>(Depth: %d)</i></font>", message.getDepth());
        headerText += "</html>";
        headerPanel.setInfoText(headerText);
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
                contentPartsPanel.remove(panel);
            }
        }

        // 2. Ensure all parts have panels and are in the correct order
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
                
                if (i >= contentPartsPanel.getComponentCount() || contentPartsPanel.getComponent(i) != panel) {
                    contentPartsPanel.add(panel, i);
                }
            }
        }

        // 3. Clean up any trailing components and re-add glue
        while (contentPartsPanel.getComponentCount() > currentParts.size()) {
            contentPartsPanel.remove(contentPartsPanel.getComponentCount() - 1);
        }
        contentPartsPanel.add(Box.createVerticalGlue());
        
        contentPartsPanel.revalidate();
        contentPartsPanel.repaint();
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
     *
     * @return The start color.
     */
    protected abstract Color getHeaderStartColor();
    /**
     * Gets the end color for the header gradient.
     *
     * @return The end color.
     */
    protected abstract Color getHeaderEndColor();
    /**
     * Gets the foreground color for the header text.
     *
     * @return The foreground color.
     */
    protected abstract Color getHeaderForegroundColor();
    /**
     * Gets the border for the message panel.
     *
     * @return The border.
     */
    protected abstract Border getMessageBorder();
}
