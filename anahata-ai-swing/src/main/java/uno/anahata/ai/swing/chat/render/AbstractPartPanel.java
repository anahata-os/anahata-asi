/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.time.Instant;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.Painter;
import uno.anahata.ai.internal.TextUtils;
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.ThoughtSignature;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;

/**
 * The abstract base class for rendering {@link AbstractPart} instances in a collapsible,
 * diff-based UI component. It extends {@link JXTitledPanel} to provide a
 * header with part metadata and pruning controls, and a content area that
 * dynamically renders the part's specific content.
 *
 * @author pablo
 * @param <T> The concrete type of AbstractPart that this panel renders.
 */
@Slf4j
@Getter
public abstract class AbstractPartPanel<T extends AbstractPart> extends JXTitledPanel {

    /**
     * The parent chat panel.
     */
    protected final ChatPanel chatPanel;
    /**
     * The part to be rendered.
     */
    protected final T part;
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
     * Button to remove the part from the message.
     */
    private final JButton removeButton;

    /**
     * Content area for the part's specific rendering.
     */
    protected final JPanel contentPanel;

    /**
     * Constructs a new AbstractPartPanel.
     *
     * @param chatPanel The parent chat panel.
     * @param part The part to be rendered.
     */
    public AbstractPartPanel(@NonNull ChatPanel chatPanel, @NonNull T part) {
        this.chatPanel = chatPanel;
        this.part = part;
        this.chatConfig = chatPanel.getChatConfig();

        // Configure JXTitledPanel
        setTitle(part.getClass().getSimpleName());
        setOpaque(false);
        setBorder(getPartBorder());

        // Custom header for rich display
        setAlpha(0.9f);
        setPaintBorderInsets(true);

        // Initialize GradientHeaderPanel
        this.headerPanel = new GradientHeaderPanel(getHeaderStartColor(), getHeaderEndColor(), getHeaderForegroundColor());
        this.pruningToggleButton = new PruningToggleButton(part);

        // Initialize Remove Button
        this.removeButton = new JButton(IconUtils.getIcon("delete.png"));
        this.removeButton.setToolTipText("Remove Part");
        this.removeButton.setMargin(new java.awt.Insets(0, 4, 0, 4));
        this.removeButton.addActionListener(e -> part.remove());

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

        // Initialize the dedicated content panel
        this.contentPanel = new JPanel();
        this.contentPanel.setLayout(new BoxLayout(this.contentPanel, BoxLayout.Y_AXIS));
        this.contentPanel.setOpaque(false);
        this.contentPanel.setBorder(BorderFactory.createEmptyBorder(10, 12, 10, 12));
        
        setContentContainer(this.contentPanel);

        // Declarative, thread-safe binding to all part properties
        new EdtPropertyChangeListener(this, part, null, evt -> render());
    }

    /**
     * Called when the component is added to its parent. Performs the initial render.
     */
    @Override
    public void addNotify() {
        super.addNotify();
        render(); // Perform initial render when added to the UI hierarchy
    }

    /**
     * Renders or updates the entire part panel, including its header and content.
     */
    public final void render() {
        updateHeaderInfoText();
        renderContent();
        updateContentVisibility();
        revalidate();
        repaint();
    }

    /**
     * Updates the text displayed in the header's info label.
     */
    protected void updateHeaderInfoText() {
        String partType = part.getClass().getSimpleName();
        String rawText = part.asText();
        String summary = TextUtils.formatValue(rawText);

        StringBuilder sb = new StringBuilder("<html>");
        sb.append(String.format("<b>%S</b> <span style='color: #AAAAAA;'>%s</span>", partType, summary));
        
        sb.append(" <font color='#666666'>- ").append(TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(part.getMessage().getTimestamp()))).append("</font>");
        sb.append(String.format(" <font color='#888888'><i>(Turns Left: %d)</i></font>", part.getTurnsLeft()));

        if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
            String sig = TextUtils.formatValue(new String(ts.getThoughtSignature()));
            sb.append(String.format("<br/><font color='#00AA00'>[Thought: %s]</font>", sig));
        }
        
        sb.append("</html>");
        headerPanel.setInfoText(sb.toString());
    }

    /**
     * Template method for subclasses to render their specific content.
     */
    protected abstract void renderContent();

    /**
     * Updates the visibility of the content panel based on the part's pruned state.
     */
    protected void updateContentVisibility() {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        boolean shouldShowContent = !isEffectivelyPruned || chatConfig.isShowPrunedParts();
        contentPanel.setVisible(shouldShowContent);
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
     * Gets the border for the part panel.
     *
     * @return The border.
     */
    protected abstract Border getPartBorder();
}
