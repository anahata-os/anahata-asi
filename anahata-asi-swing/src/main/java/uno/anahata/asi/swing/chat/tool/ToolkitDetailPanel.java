/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
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
    private final JPanel thisSysTab;
    private final JPanel childrenSysTab;
    private final JPanel thisRagTab;
    private final JPanel childrenRagTab;

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
        thisSysTab = new JPanel(new BorderLayout());
        childrenSysTab = new JPanel(new BorderLayout());
        thisRagTab = new JPanel(new BorderLayout());
        childrenRagTab = new JPanel(new BorderLayout());
        
        tabbedPane.addTab("System: This Provider", thisSysTab);
        tabbedPane.addTab("System: Children (Aggregated)", childrenSysTab);
        tabbedPane.addTab("RAG: This Provider", thisRagTab);
        tabbedPane.addTab("RAG: Children (Aggregated)", childrenRagTab);
        
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

        ContextProvider cp = toolkit.getContextProvider();
        if (cp == null) {
            // Fallback to ToolManager if the toolkit itself doesn't provide context
            cp = toolkit.getToolManager();
        }

        if (cp != null) {
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
        
        // 1. System: This Provider
        UserMessage thisSysMsg = new UserMessage(chat);
        try {
            List<String> instructions = cp.getSystemInstructions(chat);
            if (!instructions.isEmpty()) {
                new TextPart(thisSysMsg, cp.getHeader());
                for (String part : instructions) {
                    new TextPart(thisSysMsg, part);
                }
            }
        } catch (Exception e) {
            new TextPart(thisSysMsg, "**Error generating system instructions for " + cp.getName() + ":**\n" + ExceptionUtils.getStackTrace(e));
            log.error("Error generating system instructions preview for {}", cp.getName(), e);
        }
        
        if (thisSysMsg.getParts().isEmpty()) {
            new TextPart(thisSysMsg, "*No system instructions contributed by this provider.*");
        }
        
        OtherMessagePanel thisSysPanel = new OtherMessagePanel(parentPanel.getChatPanel(), thisSysMsg, false, false);
        thisSysPanel.render();
        thisSysTab.removeAll();
        thisSysTab.add(thisSysPanel, BorderLayout.CENTER);

        // 2. System: Children (Aggregated)
        UserMessage childrenSysMsg = new UserMessage(chat);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    List<String> instructions = p.getSystemInstructions(chat);
                    if (!instructions.isEmpty()) {
                        new TextPart(childrenSysMsg, p.getHeader());
                        for (String part : instructions) {
                            new TextPart(childrenSysMsg, part);
                        }
                    }
                } catch (Exception e) {
                    new TextPart(childrenSysMsg, "**Error generating system instructions for " + p.getName() + ":**\n" + ExceptionUtils.getStackTrace(e));
                    log.error("Error generating system instructions preview for child {}", p.getName(), e);
                }
            }
        }
        
        if (childrenSysMsg.getParts().isEmpty()) {
            new TextPart(childrenSysMsg, "*No system instructions contributed by children.*");
        }
        
        OtherMessagePanel childrenSysPanel = new OtherMessagePanel(parentPanel.getChatPanel(), childrenSysMsg, false, false);
        childrenSysPanel.render();
        childrenSysTab.removeAll();
        childrenSysTab.add(childrenSysPanel, BorderLayout.CENTER);

        // 3. RAG: This Provider
        RagMessage thisRagMsg = new RagMessage(chat, false);
        try {
            thisRagMsg.addPart(cp.getHeader());
            cp.populateMessage(thisRagMsg);
        } catch (Exception e) {
            new TextPart(thisRagMsg, "**Error populating RAG message for " + cp.getName() + ":**\n" + ExceptionUtils.getStackTrace(e));
            log.error("Error generating RAG content preview for {}", cp.getName(), e);
        }
        
        if (thisRagMsg.getParts().isEmpty()) {
             new TextPart(thisRagMsg, "*No RAG content contributed by this provider.*");
        }

        OtherMessagePanel thisRagPanel = new OtherMessagePanel(parentPanel.getChatPanel(), thisRagMsg, false, false);
        thisRagPanel.render();
        thisRagTab.removeAll();
        thisRagTab.add(thisRagPanel, BorderLayout.CENTER);

        // 4. RAG: Children (Aggregated)
        RagMessage childrenRagMsg = new RagMessage(chat, false);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    childrenRagMsg.addPart(p.getHeader());
                    p.populateMessage(childrenRagMsg);
                } catch (Exception e) {
                    new TextPart(childrenRagMsg, "**Error populating RAG message for " + p.getName() + ":**\n" + ExceptionUtils.getStackTrace(e));
                    log.error("Error generating RAG content preview for child {}", p.getName(), e);
                }
            }
        }
        
        if (childrenRagMsg.getParts().isEmpty()) {
             new TextPart(childrenRagMsg, "*No RAG content contributed by children.*");
        }

        OtherMessagePanel childrenRagPanel = new OtherMessagePanel(parentPanel.getChatPanel(), childrenRagMsg, false, false);
        childrenRagPanel.render();
        childrenRagTab.removeAll();
        childrenRagTab.add(childrenRagPanel, BorderLayout.CENTER);
        
        thisSysTab.revalidate();
        thisSysTab.repaint();
        childrenSysTab.revalidate();
        childrenSysTab.repaint();
        thisRagTab.revalidate();
        thisRagTab.repaint();
        childrenRagTab.revalidate();
        childrenRagTab.repaint();
    }
}
