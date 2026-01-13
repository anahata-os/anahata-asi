/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import lombok.NonNull;
import net.miginfocom.swing.MigLayout;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.icons.SearchIcon;

/**
 * A "sticky note" style card representing an active AI session.
 * It provides a visual summary of the session's state and metrics.
 * 
 * @author anahata-gemini-pro-2.5
 */
public class SessionCard extends JPanel {

    private final Chat chat;
    private final SessionsPanel.SessionController controller;
    private final JLabel nameLabel;
    private final JLabel summaryLabel;
    private final PropertyChangeListener chatListener = this::handleChatChange;

    public SessionCard(@NonNull Chat chat, @NonNull SessionsPanel.SessionController controller) {
        this.chat = chat;
        this.controller = controller;

        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(220, 180));
        setBackground(new Color(255, 253, 208)); // Pale yellow "sticky note" color
        
        // Subtle "shadow" effect via compound border
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 0, 3, 3, new Color(0, 0, 0, 30)), // Shadow
                BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(new Color(220, 220, 180), 1),
                    BorderFactory.createEmptyBorder(10, 10, 10, 10)
                )
        ));

        // Header: Nickname and Close Button
        JPanel header = new JPanel(new BorderLayout());
        header.setOpaque(false);
        
        nameLabel = new JLabel(chat.getNickname());
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        header.add(nameLabel, BorderLayout.CENTER);

        JButton closeBtn = new JButton(new DeleteIcon(14));
        closeBtn.setToolTipText("Close Session Tab");
        closeBtn.setBorderPainted(false);
        closeBtn.setContentAreaFilled(false);
        closeBtn.setFocusable(false);
        closeBtn.addActionListener(e -> controller.close(chat));
        header.add(closeBtn, BorderLayout.EAST);

        add(header, BorderLayout.NORTH);

        // Content: Status, Summary and Metrics
        JPanel content = new JPanel(new MigLayout("fillx, insets 0, gap 4", "[grow]", "[]0[]5[]0[]"));
        content.setOpaque(false);

        ChatStatus status = chat.getStatusManager().getCurrentStatus();
        JLabel statusLabel = new JLabel(status.getDisplayName());
        statusLabel.setForeground(SwingChatConfig.getColor(status));
        statusLabel.setFont(statusLabel.getFont().deriveFont(Font.ITALIC, 11f));
        content.add(statusLabel, "wrap");

        summaryLabel = new JLabel("<html><i>" + (chat.getSummary() != null ? chat.getSummary() : "No summary available.") + "</i></html>");
        summaryLabel.setFont(summaryLabel.getFont().deriveFont(11f));
        summaryLabel.setForeground(new Color(80, 80, 50));
        content.add(summaryLabel, "growx, wrap, h 40!");

        content.add(new JLabel("Messages: " + chat.getContextManager().getHistory().size()), "wrap");
        
        double usage = chat.getContextWindowUsage();
        JLabel usageLabel = new JLabel("Context: " + String.format("%.1f%%", usage * 100));
        usageLabel.setForeground(SwingChatConfig.getColorForContextUsage(usage));
        content.add(usageLabel, "wrap");

        add(content, BorderLayout.CENTER);

        // Footer: ID and Focus Button
        JPanel footer = new JPanel(new BorderLayout());
        footer.setOpaque(false);
        
        JLabel idLabel = new JLabel(chat.getShortId());
        idLabel.setFont(idLabel.getFont().deriveFont(10f));
        idLabel.setForeground(Color.GRAY);
        footer.add(idLabel, BorderLayout.WEST);
        
        JButton focusBtn = new JButton("Focus", new SearchIcon(14));
        focusBtn.setToolTipText("Open/Focus this session tab");
        focusBtn.setMargin(new java.awt.Insets(2, 4, 2, 4));
        focusBtn.setFont(focusBtn.getFont().deriveFont(10f));
        focusBtn.addActionListener(e -> controller.focus(chat));
        footer.add(focusBtn, BorderLayout.EAST);
        
        add(footer, BorderLayout.SOUTH);

        // Interaction
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 1) {
                    controller.focus(chat);
                }
            }
            
            @Override
            public void mouseEntered(MouseEvent e) {
                setBackground(new Color(255, 255, 225));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                setBackground(new Color(255, 253, 208));
            }
        });
        
        chat.addPropertyChangeListener(chatListener);
    }

    private void handleChatChange(PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        if ("nickname".equals(prop)) {
            SwingUtilities.invokeLater(() -> nameLabel.setText((String) evt.getNewValue()));
        } else if ("summary".equals(prop)) {
            SwingUtilities.invokeLater(() -> summaryLabel.setText("<html><i>" + evt.getNewValue() + "</i></html>"));
        }
    }
    
    public void cleanup() {
        chat.removePropertyChangeListener(chatListener);
    }
}
