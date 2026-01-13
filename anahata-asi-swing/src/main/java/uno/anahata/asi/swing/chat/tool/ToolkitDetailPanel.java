/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ToolsPanel;
import uno.anahata.asi.swing.chat.render.OtherMessagePanel;

/**
 * A panel that displays the details and context previews for a specific {@link AbstractToolkit}.
 * It uses the existing message rendering infrastructure to show "System Instructions" and "RAG Content"
 * as they would appear to the model.
 *
 * @author anahata
 */
@Slf4j
public class ToolkitDetailPanel extends JPanel {

    private final ToolsPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JCheckBox enabledCheckbox;
    
    private final JPanel contentPanel;
    private final CardLayout contentLayout;
    private final JTabbedPane tabbedPane;
    private final JPanel sysTab;
    private final JPanel ragTab;

    /**
     * Constructs a new ToolkitDetailPanel.
     * @param parentPanel The parent ToolsPanel.
     */
    public ToolkitDetailPanel(ToolsPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        // Header Panel
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Toolkit Details"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(java.awt.Font.BOLD, 14f));
        headerPanel.add(nameLabel, gbc);
        gbc.gridy++;

        descLabel = new JLabel();
        headerPanel.add(descLabel, gbc);
        gbc.gridy++;

        enabledCheckbox = new JCheckBox("Enabled");
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        headerPanel.add(enabledCheckbox, gbc);

        add(headerPanel, BorderLayout.NORTH);

        // Content Panel with CardLayout
        contentLayout = new CardLayout();
        contentPanel = new JPanel(contentLayout);
        
        tabbedPane = new JTabbedPane();
        sysTab = new JPanel(new BorderLayout());
        ragTab = new JPanel(new BorderLayout());
        tabbedPane.addTab("System Instructions", sysTab);
        tabbedPane.addTab("RAG Content (Live Workspace)", ragTab);
        
        contentPanel.add(tabbedPane, "tabs");
        contentPanel.add(new JLabel("This toolkit does not provide additional context.", SwingConstants.CENTER), "empty");
        
        add(contentPanel, BorderLayout.CENTER);
    }

    /**
     * Updates the panel to display the details for the given toolkit.
     *
     * @param toolkit The selected toolkit.
     */
    public void setToolkit(AbstractToolkit toolkit) {
        nameLabel.setText("Toolkit: " + toolkit.getName());
        descLabel.setText("<html>" + toolkit.getDescription().replace("\n", "<br>") + "</html>");
        
        // Remove existing listeners to avoid multiple triggers
        for (java.awt.event.ActionListener al : enabledCheckbox.getActionListeners()) {
            enabledCheckbox.removeActionListener(al);
        }
        enabledCheckbox.setSelected(toolkit.isEnabled());
        enabledCheckbox.addActionListener(e -> {
            toolkit.setEnabled(enabledCheckbox.isSelected());
            parentPanel.refresh();
            setToolkit(toolkit); // Refresh previews
        });

        if (toolkit instanceof ContextProvider cp) {
            contentLayout.show(contentPanel, "tabs");
            updatePreviews(cp);
        } else {
            contentLayout.show(contentPanel, "empty");
        }

        revalidate();
        repaint();
    }

    /**
     * Generates and renders previews of the toolkit's context contributions.
     *
     * @param cp The context provider to preview.
     */
    private void updatePreviews(ContextProvider cp) {
        Chat chat = parentPanel.getChat();
        
        // 1. System Instructions Preview
        UserMessage sysMsg = new UserMessage(chat);
        new TextPart(sysMsg, cp.getHeader());
        try {
            for (String part : cp.getSystemInstructions(chat)) {
                new TextPart(sysMsg, part);
            }
        } catch (Exception e) {
            new TextPart(sysMsg, "**Error generating system instructions:**\n" + e.getMessage());
            log.error("Error generating system instructions preview", e);
        }
        
        OtherMessagePanel sysPanel = new OtherMessagePanel(parentPanel.getChatPanel(), sysMsg, false, false);
        sysPanel.render();
        sysTab.removeAll();
        sysTab.add(sysPanel, BorderLayout.CENTER);

        // 2. RAG Content Preview
        RagMessage ragMsg = new RagMessage(chat);
        ragMsg.getParts().get(0).remove();
        
        try {
            cp.populateMessage(ragMsg);
        } catch (Exception e) {
            new TextPart(ragMsg, "**Error populating RAG message:**\n" + e.getMessage());
            log.error("Error generating RAG content preview", e);
        }
        
        if (ragMsg.getParts().size() <= 1) { // Only the header
             new TextPart(ragMsg, "*No RAG content contributed.*");
        }

        OtherMessagePanel ragPanel = new OtherMessagePanel(parentPanel.getChatPanel(), ragMsg, false, false);
        ragPanel.render();
        ragTab.removeAll();
        ragTab.add(ragPanel, BorderLayout.CENTER);
        
        sysTab.revalidate();
        sysTab.repaint();
        ragTab.revalidate();
        ragTab.repaint();
    }
}
