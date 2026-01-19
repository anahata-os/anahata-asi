/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.time.Instant;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.painter.MattePainter;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.swing.chat.SwingChatConfig.UITheme;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * The abstract base class for rendering {@link AbstractPart} instances in a 
 * diff-based UI component. It leverages {@link JXTitledPanel} to provide a
 * styled header with part metadata and pruning controls, and a content area 
 * that dynamically renders the part's specific content.
 *
 * @author anahata
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
    /** Label to display the number of turns left for this part. */
    private final JLabel turnsLeftLabel;

    /** Container for the part's specific content ("the beef"). */
    private final JXPanel centralContainer;
    /** Container for part-level metadata and actions. */
    protected final JPanel footerContainer;
    /** Container for header actions on the right. */
    protected final JPanel rightButtonPanel;
    
    /** Label for the thought signature, if present. */
    private JLabel thoughtSignatureLabel;

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
        // Lightened the background by using a higher alpha or a lighter base color
        Color startColor = new Color(248, 248, 248, 80); 
        MattePainter mp = new MattePainter(new GradientPaint(0, 0, startColor, 1, 0, new Color(0,0,0,0)), true);
        setTitlePainter(mp);

        // 2. Initialize Header Buttons and Labels
        this.pruningToggleButton = new PruningToggleButton(part);
        
        this.copyButton = new JButton(new CopyIcon(14));
        this.copyButton.setToolTipText("Copy Part Content");
        this.copyButton.setMargin(new java.awt.Insets(0, 2, 0, 2));
        this.copyButton.addActionListener(e -> copyToClipboard());

        this.removeButton = new JButton(new DeleteIcon(14));
        this.removeButton.setToolTipText("Remove Part");
        this.removeButton.setMargin(new java.awt.Insets(0, 2, 0, 2));
        this.removeButton.addActionListener(e -> part.remove());
        
        this.turnsLeftLabel = new JLabel();
        this.turnsLeftLabel.setFont(new Font("SansSerif", Font.ITALIC, 10));
        this.turnsLeftLabel.setForeground(new Color(160, 160, 160));

        // Copy button on the left
        setLeftDecoration(copyButton);

        // Actions panel on the right
        this.rightButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        this.rightButtonPanel.setOpaque(false);
        this.rightButtonPanel.add(turnsLeftLabel);
        this.rightButtonPanel.add(pruningToggleButton);
        this.rightButtonPanel.add(removeButton);
        setRightDecoration(rightButtonPanel);

        // 3. Setup Content Layout
        JXPanel mainContainer = (JXPanel) getContentContainer();
        mainContainer.setLayout(new BorderLayout());
        mainContainer.setOpaque(false);
        mainContainer.setBorder(BorderFactory.createEmptyBorder(2, 8, 5, 8));

        this.centralContainer = new JXPanel();
        this.centralContainer.setLayout(new BoxLayout(this.centralContainer, BoxLayout.Y_AXIS));
        this.centralContainer.setOpaque(false);
        mainContainer.add(this.centralContainer, BorderLayout.CENTER);

        this.footerContainer = new JPanel();
        this.footerContainer.setLayout(new BoxLayout(this.footerContainer, BoxLayout.Y_AXIS));
        this.footerContainer.setOpaque(false);
        mainContainer.add(this.footerContainer, BorderLayout.SOUTH);

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
        updateHeaderButtons();
        renderContent();
        renderFooterInternal();
        updateContentVisibility();
        revalidate();
        repaint();
    }

    /**
     * Updates the visibility of header buttons based on the parent message panel's flags.
     */
    protected void updateHeaderButtons() {
        boolean prune = true;
        boolean remove = true;
        
        AbstractMessagePanel parentMessagePanel = (AbstractMessagePanel) SwingUtilities.getAncestorOfClass(AbstractMessagePanel.class, this);
        if (parentMessagePanel != null) {
            prune = parentMessagePanel.isRenderPruneButtons();
            remove = parentMessagePanel.isRenderRemoveButtons();
        }

        pruningToggleButton.setVisible(prune);
        removeButton.setVisible(remove);
        turnsLeftLabel.setVisible(prune);
    }

    /**
     * Updates the text displayed in the header's title.
     */
    protected void updateHeaderInfoText() {
        String rawText = part.asText();
        // Fix: Handle null rawText to avoid "null" string in summary
        String summary = (rawText == null || rawText.isEmpty()) ? "" : TextUtils.formatValue(rawText);
        
        log.info("Updating header info text for part: {}", summary);

        StringBuilder sb = new StringBuilder("<html>");
        sb.append(String.format("<span style='color: #888888;'>%s</span>", summary));
        
        // The message reference is now guaranteed to be present by the architectural fix in AbstractMessage.addPart.
        sb.append(" <font color='#999999' size='2'>- ").append(TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(part.getMessage().getTimestamp()))).append("</font>");
        sb.append("</html>");
        
        String newTitle = sb.toString();
        if (!newTitle.equals(getTitle())) {
            setTitle(newTitle);
        }
        
        // Update turns left label in the action panel
        turnsLeftLabel.setText(String.valueOf(part.getTurnsLeft()));
    }

    private void renderFooterInternal() {
        if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
            String sig = TextUtils.formatValue(ts.getThoughtSignature());
            if (thoughtSignatureLabel == null) {
                thoughtSignatureLabel = new JLabel();
                thoughtSignatureLabel.setBorder(BorderFactory.createEmptyBorder(2, 0, 2, 0));
                footerContainer.add(thoughtSignatureLabel);
            }
            thoughtSignatureLabel.setText(String.format("<html><font color='#888888' size='2'>Thought Signature: %s</font></html>", sig));
        }
        
        renderFooter();
    }

    /**
     * Template method for subclasses to add components to the part footer.
     * Subclasses should use the {@code footerContainer} field directly.
     */
    protected void renderFooter() {
        // Default implementation does nothing
    }

    /**
     * Template method for subclasses to render their specific content.
     * Subclasses should use the {@code centralContainer} field directly.
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
