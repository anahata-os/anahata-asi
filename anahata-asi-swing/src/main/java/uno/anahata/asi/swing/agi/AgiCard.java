/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j; 
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.status.AgiStatus;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.SearchIcon;

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
    /** The controller for handling session actions. */
    private final SessionController controller;
    /** The label displaying the agi's nickname. */
    private final JLabel nameLabel;
    /** The text area displaying the agi's summary. */
    private final JTextArea summaryArea;
    /** The listener for agi property changes. */
    private final PropertyChangeListener agiListener = this::handleAgiChange;

    @Getter
    private boolean selected = false;
    
    private final SwingAgiConfig.UITheme theme;

    /**
     * Constructs a new AgiCard for the given agi session.
     * 
     * @param agi The agi session to represent.
     * @param controller The controller for session actions.
     */
    public AgiCard(@NonNull Agi agi, @NonNull SessionController controller) {
        this.agi = agi;
        this.controller = controller;
        this.theme = ((SwingAgiConfig)agi.getConfig()).getTheme();

        // Use MigLayout for the whole card to control vertical growth
        setLayout(new MigLayout("fillx, insets 10, gap 4", "[grow]", "[]5[]5[]"));
        setBackground(theme.getCardNormalBg());
        
        updateBorder();

        // Header: Nickname and Close Button
        JPanel header = new JPanel(new MigLayout("fillx, insets 0", "[grow][]", "[]"));
        header.setOpaque(false);
        
        nameLabel = new JLabel(agi.getDisplayName());
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        header.add(nameLabel, "growx");

        JButton closeBtn = new JButton(new DeleteIcon(14));
        closeBtn.setToolTipText("Close Session Tab");
        closeBtn.setBorderPainted(false);
        closeBtn.setContentAreaFilled(false);
        closeBtn.setFocusable(false);
        closeBtn.addActionListener(e -> controller.close(agi));
        header.add(closeBtn, "w 20!, h 20!");

        add(header, "growx, wrap");

        // Content: Status, Summary and Metrics
        JPanel content = new JPanel(new MigLayout("fillx, insets 0, gap 4", "[grow]", "[]0[]5[]0[]"));
        content.setOpaque(false);

        AgiStatus status = agi.getStatusManager().getCurrentStatus();
        JLabel statusLabel = new JLabel(status.getDisplayName());
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

        content.add(new JLabel("Messages: " + agi.getContextManager().getHistory().size()), "wrap");
        
        double usage = agi.getContextWindowUsage();
        JLabel usageLabel = new JLabel("Context: " + String.format("%.1f%%", usage * 100));
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
        
        JButton focusBtn = new JButton("Focus", new SearchIcon(14));
        focusBtn.setToolTipText("Open/Focus this session tab");
        focusBtn.setMargin(new Insets(2, 4, 2, 4));
        focusBtn.setFont(focusBtn.getFont().deriveFont(10f));
        focusBtn.addActionListener(e -> controller.focus(agi));
        footer.add(focusBtn);
        
        add(footer, "growx");

        // Interaction
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    controller.focus(agi);
                }
            }
            
            @Override
            public void mouseEntered(MouseEvent e) {
                if (!selected) setBackground(theme.getCardHoverBg());
            }

            @Override
            public void mouseExited(MouseEvent e) {
                if (!selected) setBackground(theme.getCardNormalBg());
            }
        });
        
        agi.addPropertyChangeListener(agiListener);
    }

    @Override
    public Dimension getPreferredSize() {
        Dimension d = super.getPreferredSize();
        d.width = 250; // Maintain consistent width
        return d;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
        setBackground(selected ? theme.getCardSelectedBg() : theme.getCardNormalBg());
        updateBorder();
        repaint();
    }

    private void updateBorder() {
        Color borderColor = selected ? theme.getCardSelectedBorder() : theme.getCardBorder();
        int thickness = selected ? 2 : 1;
        
        Border lineBorder = BorderFactory.createLineBorder(borderColor, thickness);
        Border shadowBorder = BorderFactory.createMatteBorder(0, 0, 3, 3, new Color(0, 0, 0, 30));
        Border marginBorder = BorderFactory.createEmptyBorder(0, 0, 0, 0); // MigLayout handles insets
        
        setBorder(BorderFactory.createCompoundBorder(shadowBorder, BorderFactory.createCompoundBorder(lineBorder, marginBorder)));
    }

    /**
     * Handles property change events from the agi session.
     * 
     * @param evt The property change event.
     */
    private void handleAgiChange(PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        if ("nickname".equals(prop)) {
            SwingUtilities.invokeLater(() -> {
                nameLabel.setText(agi.getDisplayName());
                revalidate();
                repaint();
            });
        } else if ("summary".equals(prop)) {
            SwingUtilities.invokeLater(() -> {
                String summary = (String) evt.getNewValue();
                summaryArea.setText(summary != null ? summary : "No summary available.");
                revalidate();
                repaint();
            });
        }
    }
    
    /**
     * Cleans up resources and removes listeners. Should be called when the card
     * is no longer needed.
     */
    public void cleanup() {
        agi.removePropertyChangeListener(agiListener);
    }
}
