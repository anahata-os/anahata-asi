/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.status.ChatStatus;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.SearchIcon;

/**
 * A "sticky note" style card representing an active AI session.
 * It provides a visual summary of the session's state, metrics, and quick actions.
 * This component listens for property changes on the {@link Chat} object to
 * update its display in real-time.
 * 
 * @author anahata
 */
@Slf4j
public class ChatCard extends JPanel {

    /** The chat session represented by this card. */
    @Getter
    private final Chat chat;
    /** The controller for handling session actions. */
    private final SessionController controller;
    /** The label displaying the chat's nickname. */
    private final JLabel nameLabel;
    /** The text area displaying the chat's summary. */
    private final JTextArea summaryArea;
    /** The listener for chat property changes. */
    private final PropertyChangeListener chatListener = this::handleChatChange;

    @Getter
    private boolean selected = false;
    
    private final SwingChatConfig.UITheme theme;

    /**
     * Constructs a new ChatCard for the given chat session.
     * 
     * @param chat The chat session to represent.
     * @param controller The controller for session actions.
     */
    public ChatCard(@NonNull Chat chat, @NonNull SessionController controller) {
        this.chat = chat;
        this.controller = controller;
        this.theme = ((SwingChatConfig)chat.getConfig()).getTheme();

        // Use MigLayout for the whole card to control vertical growth
        setLayout(new MigLayout("fillx, insets 10, gap 4", "[grow]", "[]5[]5[]"));
        setBackground(theme.getCardNormalBg());
        
        updateBorder();

        // Header: Nickname and Close Button
        JPanel header = new JPanel(new MigLayout("fillx, insets 0", "[grow][]", "[]"));
        header.setOpaque(false);
        
        nameLabel = new JLabel(chat.getNickname());
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        header.add(nameLabel, "growx");

        JButton closeBtn = new JButton(new DeleteIcon(14));
        closeBtn.setToolTipText("Close Session Tab");
        closeBtn.setBorderPainted(false);
        closeBtn.setContentAreaFilled(false);
        closeBtn.setFocusable(false);
        closeBtn.addActionListener(e -> controller.close(chat));
        header.add(closeBtn, "w 20!, h 20!");

        add(header, "growx, wrap");

        // Content: Status, Summary and Metrics
        JPanel content = new JPanel(new MigLayout("fillx, insets 0, gap 4", "[grow]", "[]0[]5[]0[]"));
        content.setOpaque(false);

        ChatStatus status = chat.getStatusManager().getCurrentStatus();
        JLabel statusLabel = new JLabel(status.getDisplayName());
        statusLabel.setForeground(SwingChatConfig.getColor(status));
        statusLabel.setFont(statusLabel.getFont().deriveFont(Font.ITALIC, 11f));
        content.add(statusLabel, "wrap");

        summaryArea = new JTextArea(chat.getConversationSummary() != null ? chat.getConversationSummary() : "No summary available.");
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

        content.add(new JLabel("Messages: " + chat.getContextManager().getHistory().size()), "wrap");
        
        double usage = chat.getContextWindowUsage();
        JLabel usageLabel = new JLabel("Context: " + String.format("%.1f%%", usage * 100));
        usageLabel.setForeground(SwingChatConfig.getColorForContextUsage(usage));
        content.add(usageLabel, "wrap");

        add(content, "growx, wrap");

        // Footer: ID and Focus Button
        JPanel footer = new JPanel(new MigLayout("fillx, insets 0", "[grow][]", "[]"));
        footer.setOpaque(false);
        
        JLabel idLabel = new JLabel(chat.getShortId());
        idLabel.setFont(idLabel.getFont().deriveFont(10f));
        idLabel.setForeground(Color.GRAY);
        footer.add(idLabel, "growx");
        
        JButton focusBtn = new JButton("Focus", new SearchIcon(14));
        focusBtn.setToolTipText("Open/Focus this session tab");
        focusBtn.setMargin(new Insets(2, 4, 2, 4));
        focusBtn.setFont(focusBtn.getFont().deriveFont(10f));
        focusBtn.addActionListener(e -> controller.focus(chat));
        footer.add(focusBtn);
        
        add(footer, "growx");

        // Interaction
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    controller.focus(chat);
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
        
        chat.addPropertyChangeListener(chatListener);
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
     * Handles property change events from the chat session.
     * 
     * @param evt The property change event.
     */
    private void handleChatChange(PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        if ("nickname".equals(prop)) {
            SwingUtilities.invokeLater(() -> {
                nameLabel.setText((String) evt.getNewValue());
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
        chat.removePropertyChangeListener(chatListener);
    }
}
