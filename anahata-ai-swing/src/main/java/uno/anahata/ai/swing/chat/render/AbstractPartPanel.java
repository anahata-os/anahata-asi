/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.time.Instant;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.MattePainter;
import uno.anahata.ai.internal.TextUtils;
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.ThoughtSignature;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig;
import uno.anahata.ai.swing.chat.SwingChatConfig.UITheme;
import uno.anahata.ai.swing.icons.CopyIcon;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;
import uno.anahata.ai.swing.internal.SwingUtils;

/**
 * The abstract base class for rendering {@link AbstractPart} instances in a 
 * diff-based UI component. It leverages {@link JXTitledPanel} to provide a
 * styled header with part metadata and pruning controls, and a content area 
 * that dynamically renders the part's specific content.
 *
 * @author pablo
 * @param <T> The concrete type of AbstractPart that this panel renders.
 */
@Slf4j
@Getter
public abstract class AbstractPartPanel<T extends AbstractPart> extends JXTitledPanel {

    /** The parent chat panel. */
    protected final ChatPanel chatPanel;
    /** The part to be rendered. */
    protected final T part;
    /** The chat configuration. */
    protected final SwingChatConfig chatConfig;

    /** Toggle button for pruning control. */
    private final PruningToggleButton pruningToggleButton;
    /** Button to copy the part content to the clipboard. */
    private final JButton copyButton;
    /** Button to remove the part from the message. */
    private final JButton removeButton;

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
        UITheme theme = chatConfig.getTheme();

        // 1. Configure JXTitledPanel Header (Faint and Role-Neutral)
        setTitleForeground(theme.getPartHeaderFg());
        setTitleFont(new Font("SansSerif", Font.PLAIN, 11));

        // Background Gradient via MattePainter (Faint)
        MattePainter mp = new MattePainter(new GradientPaint(0, 0, theme.getPartHeaderBg(), 1, 0, new Color(0,0,0,0)), true);
        setTitlePainter(mp);

        // 2. Initialize Header Buttons
        this.pruningToggleButton = new PruningToggleButton(part);
        
        this.copyButton = new JButton(new CopyIcon(14));
        this.copyButton.setToolTipText("Copy Part Content");
        this.copyButton.setMargin(new java.awt.Insets(0, 2, 0, 2));
        this.copyButton.addActionListener(e -> copyToClipboard());

        this.removeButton = new JButton(new DeleteIcon(14));
        this.removeButton.setToolTipText("Remove Part");
        this.removeButton.setMargin(new java.awt.Insets(0, 2, 0, 2));
        this.removeButton.addActionListener(e -> part.remove());

        // Copy button on the left
        setLeftDecoration(copyButton);

        // Pruning and Remove buttons on the right
        JPanel rightButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 2, 0));
        rightButtonPanel.setOpaque(false);
        rightButtonPanel.add(pruningToggleButton);
        rightButtonPanel.add(removeButton);
        setRightDecoration(rightButtonPanel);

        // 3. Setup Content Container (Using the default JXPanel provided by JXTitledPanel)
        JXPanel mainContainer = (JXPanel) getContentContainer();
        mainContainer.setLayout(new BoxLayout(mainContainer, BoxLayout.Y_AXIS));
        mainContainer.setOpaque(false);
        mainContainer.setBorder(BorderFactory.createEmptyBorder(2, 8, 5, 8));

        setOpaque(false);
        setBorder(BorderFactory.createLineBorder(theme.getPartBorder(), 1, true));

        // 4. Expand/Collapse Logic on Header Click
        if (getComponentCount() > 0) {
            getComponent(0).addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    toggleExpanded();
                }
            });
        }

        // Declarative, thread-safe binding to all part properties
        new EdtPropertyChangeListener(this, part, null, evt -> render());
    }

    /**
     * Copies the part's content to the system clipboard.
     * Subclasses can override this to provide specialized copy behavior.
     */
    protected void copyToClipboard() {
        SwingUtils.copyToClipboard(part.asText());
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
     * Updates the text displayed in the header's title.
     */
    protected void updateHeaderInfoText() {
        String rawText = part.asText();
        String summary = TextUtils.formatValue(rawText);
        UITheme theme = chatConfig.getTheme();

        StringBuilder sb = new StringBuilder("<html>");
        sb.append(String.format("<b>%s</b> ", part.getClass().getSimpleName()));
        sb.append(String.format("<span style='color: #888888;'>[%s]</span>", summary));
        sb.append(" <font color='#999999' size='2'>- ").append(TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(part.getMessage().getTimestamp()))).append("</font>");
        sb.append(String.format(" <font color='#AAAAAA' size='2'><i>(Turns Left: %d)</i></font>", part.getTurnsLeft()));

        if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
            String sig = TextUtils.formatValue(ts.getThoughtSignature());
            sb.append(String.format(" <font color='#888888' size='2'>[Thought: %s]</font>", sig));
        }
        
        sb.append("</html>");
        setTitle(sb.toString());
    }

    /**
     * Template method for subclasses to render their specific content.
     */
    protected abstract void renderContent();

    /**
     * Updates the visibility of the content container based on the part's pruned state.
     */
    protected void updateContentVisibility() {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        boolean shouldShowContent = !isEffectivelyPruned || chatConfig.isShowPrunedParts();
        getContentContainer().setVisible(shouldShowContent);
    }
}
