/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j; 
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A "sticky note" style card representing an active AI session.
 * It provides a visual summary of the session's state, metrics, and quick actions.
 * This component listens for property changes on the {@link Agi} object to
 * update its display in real-time.
 * 
 * @author anahata
 */
@Slf4j
public class AgiCard extends JPanel {

    /** The agi session represented by this card. */
    @Getter
    private final Agi agi;
    /** The container for handling session actions. */
    private final AbstractAsiContainer container;
    /** The label displaying the agi's nickname. */
    private final JLabel nameLabel;
    /** The text area displaying the agi's summary. */
    private final JTextArea summaryArea;
    /** The label displaying the current status of the agi. */
    private final JLabel statusLabel;
    /** The label displaying the total number of messages in the conversation. */
    private final JLabel messageCountLabel;
    /** The label displaying the context window usage percentage. */
    private final JLabel usageLabel;
    /** The button to close the session's tab/window. */
    private final JButton closeBtn;
    
    /** Reactive listeners for all session aspects. */
    /** The listener for changes in the agi session's properties. */
    private final EdtPropertyChangeListener agiListener;
    /** The listener for status changes in the agi session. */
    private final EdtPropertyChangeListener statusListener;
    /** The listener for history changes in the context manager. */
    private final EdtPropertyChangeListener historyListener;
    /** The listener for resource changes in the resource manager. */
    private final EdtPropertyChangeListener resourceListener;

    @Getter
    /** Whether the card is currently selected in the container. */
    private boolean selected = false;
    
    /** The UI theme derived from the agi's configuration. */
    private final SwingAgiConfig.UITheme theme;

    /**
     * Constructs a new AgiCard for the given agi session.
     * 
     * @param agi The agi session to represent.
     * @param container The container for session actions.
     */
    public AgiCard(@NonNull Agi agi, @NonNull AbstractAsiContainer container) {
        this.agi = agi;
        this.container = container;
        this.theme = ((SwingAgiConfig)agi.getConfig()).getTheme();

        // Use MigLayout for the whole card to control vertical growth
        setLayout(new MigLayout("fillx, insets 10, gap 4", "[grow]", "[]5[]5[]"));
        
        updateBorder();

        // Header: Nickname, Close, and Dispose Buttons
        JPanel header = new JPanel(new MigLayout("fillx, insets 0", "[grow][][]", "[]"));
        header.setOpaque(false);
        
        nameLabel = new JLabel(agi.getDisplayName());
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        header.add(nameLabel, "growx");

        closeBtn = new JButton(new CancelIcon(14));
        closeBtn.setToolTipText("Close Session Tab");
        closeBtn.setBorderPainted(false);
        closeBtn.setContentAreaFilled(false);
        closeBtn.setFocusable(false);
        closeBtn.addActionListener(e -> container.close(agi));
        header.add(closeBtn, "w 20!, h 20!");
        
        JButton disposeBtn = new JButton(new DeleteIcon(14));
        disposeBtn.setToolTipText("Permanently Dispose Session");
        disposeBtn.setBorderPainted(false);
        disposeBtn.setContentAreaFilled(false);
        disposeBtn.setFocusable(false);
        disposeBtn.addActionListener(e -> container.dispose(agi));
        header.add(disposeBtn, "w 20!, h 20!");

        add(header, "growx, wrap");

        // Content: Status, Summary and Metrics
        JPanel content = new JPanel(new MigLayout("fillx, insets 0, gap 4", "[grow]", "[]0[]5[]0[]"));
        content.setOpaque(false);

        AgiStatus status = agi.getStatusManager().getCurrentStatus();
        statusLabel = new JLabel(status.getDisplayName());
        statusLabel.setForeground(SwingAgiConfig.getColor(status));
        statusLabel.setFont(statusLabel.getFont().deriveFont(Font.ITALIC, 11f));
        content.add(statusLabel, "wrap");

        summaryArea = new JTextArea(agi.getConversationSummary() != null ? agi.getConversationSummary() : "No summary available.");
        summaryArea.setFont(summaryArea.getFont().deriveFont(Font.ITALIC, 11f));
        summaryArea.setForeground(new Color(80, 80, 50));
        summaryArea.setLineWrap(true);
        summaryArea.setWrapStyleWord(true);
        summaryArea.setEditable(false);
        summaryArea.setOpaque(false);
        summaryArea.setFocusable(false);
        summaryArea.setBorder(null);
        summaryArea.setRows(0); // Dynamic height
        summaryArea.setColumns(20);

        content.add(summaryArea, "growx, wrap, gapbottom 10");

        messageCountLabel = new JLabel("Messages: " + agi.getContextManager().getHistory().size());
        content.add(messageCountLabel, "wrap");
        
        double usage = agi.getContextWindowUsage();
        usageLabel = new JLabel("Context: " + String.format("%.1f%%", usage * 100));
        usageLabel.setForeground(SwingAgiConfig.getColorForContextUsage(usage));
        content.add(usageLabel, "wrap");

        add(content, "growx, wrap");

        // Footer: ID and Focus Button
        JPanel footer = new JPanel(new MigLayout("fillx, insets 0", "[grow][]", "[]"));
        footer.setOpaque(false);
        
        JLabel idLabel = new JLabel(agi.getShortId());
        idLabel.setFont(idLabel.getFont().deriveFont(10f));
        idLabel.setForeground(Color.GRAY);
        footer.add(idLabel, "growx");
        
        JButton focusBtn = new JButton("Open", new SearchIcon(14));
        focusBtn.setToolTipText("Open/Focus this session tab");
        focusBtn.setMargin(new Insets(2, 4, 2, 4));
        focusBtn.setFont(focusBtn.getFont().deriveFont(10f));
        focusBtn.addActionListener(e -> container.open(agi));
        footer.add(focusBtn);
        
        add(footer, "growx");

        // Interaction
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    container.open(agi);
                }
            }
            
            @Override
            public void mouseEntered(MouseEvent e) {
                updateBackground();
            }

            @Override
            public void mouseExited(MouseEvent e) {
                updateBackground();
            }
        });
        
        // WIRE UP MULTI-SOURCE REACTIVITY
        this.agiListener = new EdtPropertyChangeListener(this, agi, null, this::handleAgiChange);
        this.statusListener = new EdtPropertyChangeListener(this, agi.getStatusManager(), "currentStatus", this::handleStatusChange);
        this.historyListener = new EdtPropertyChangeListener(this, agi.getContextManager(), "history", this::handleHistoryChange);
        this.resourceListener = new EdtPropertyChangeListener(this, agi.getResourceManager(), "resources", this::handleResourceChange);
        
        syncState();
    }

    /** 
     * Synchronizes the UI state with the agi's 'open' status and updates the background.
     */
    private void syncState() {
        closeBtn.setVisible(agi.isOpen());
        updateBackground();
        repaint();
    }

    /** 
     * Updates the card's background color based on selection, hover, and open state.
     */
    private void updateBackground() {
        if (selected) {
            setBackground(theme.getCardSelectedBg());
        } else if (getMousePosition() != null) {
            setBackground(theme.getCardHoverBg());
        } else {
            // Archived (Closed) cards get a dimmed background
            setBackground(agi.isOpen() ? theme.getCardNormalBg() : new Color(240, 240, 240));
        }
    }

    @Override
    /** 
     * {@inheritDoc} 
     * <p>Returns a fixed width of 250 to ensure card consistency in the grid.</p> 
     */
    public Dimension getPreferredSize() {
        Dimension d = super.getPreferredSize();
        d.width = 250; // Maintain consistent width
        return d;
    }

    /** 
     * Sets the selection state of the card and updates its visual appearance.
     * 
     * @param selected true if the card is selected.
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
        updateBackground();
        updateBorder();
        repaint();
    }

    /** 
     * Updates the card's border thickness and color based on the selection state.
     */
    private void updateBorder() {
        Color borderColor = selected ? theme.getCardSelectedBorder() : theme.getCardBorder();
        int thickness = selected ? 2 : 1;
        
        Border lineBorder = BorderFactory.createLineBorder(borderColor, thickness);
        Border shadowBorder = BorderFactory.createMatteBorder(0, 0, 3, 3, new Color(0, 0, 0, 30));
        
        setBorder(BorderFactory.createCompoundBorder(shadowBorder, lineBorder));
    }

    /** 
     * Handles property change events from the agi session.
     * 
     * @param evt The property change event.
     */
    private void handleAgiChange(PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        if ("nickname".equals(prop)) {
            nameLabel.setText(agi.getDisplayName());
        } else if ("summary".equals(prop)) {
            summaryArea.setText(agi.getConversationSummary() != null ? agi.getConversationSummary() : "No summary available.");
        } else if ("open".equals(prop)) {
            syncState();
        }
        revalidate();
        repaint();
    }

    /** 
     * Handles status change events from the agi status manager.
     * 
     * @param evt The property change event.
     */
    private void handleStatusChange(PropertyChangeEvent evt) {
        AgiStatus status = (AgiStatus) evt.getNewValue();
        statusLabel.setText(status.getDisplayName());
        statusLabel.setForeground(SwingAgiConfig.getColor(status));
    }

    /** 
     * Handles history change events from the context manager.
     * 
     * @param evt The property change event.
     */
    private void handleHistoryChange(PropertyChangeEvent evt) {
        messageCountLabel.setText("Messages: " + agi.getContextManager().getHistory().size());
        updateMetrics();
    }

    /** 
     * Handles resource change events from the resource manager.
     * 
     * @param evt The property change event.
     */
    private void handleResourceChange(PropertyChangeEvent evt) {
        updateMetrics();
    }

    /** 
     * Recalculates and updates the labels for message count and context usage.
     */
    private void updateMetrics() {
        double usage = agi.getContextWindowUsage();
        usageLabel.setText("Context: " + String.format("%.1f%%", usage * 100));
        usageLabel.setForeground(SwingAgiConfig.getColorForContextUsage(usage));
    }
    
    /**
     * Cleans up resources and removes listeners. Should be called when the card
     * is no longer needed.
     */
    public void cleanup() {
        agiListener.unbind();
        statusListener.unbind();
        historyListener.unbind();
        resourceListener.unbind();
    }
}
