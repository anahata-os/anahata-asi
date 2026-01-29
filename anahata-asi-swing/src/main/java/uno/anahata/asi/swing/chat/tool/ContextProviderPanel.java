/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
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
import uno.anahata.asi.swing.chat.ContextPanel;
import uno.anahata.asi.swing.chat.render.OtherMessagePanel;

/**
 * A panel that displays the details and context previews for a {@link ContextProvider}.
 *
 * @author anahata
 */
@Slf4j
public class ContextProviderPanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JCheckBox enabledCheckbox;
    
    private final JTabbedPane tabbedPane;
    private final JPanel thisSysTab;
    private final JPanel childrenSysTab;
    private final JPanel thisRagTab;
    private final JPanel childrenRagTab;

    public ContextProviderPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        // Header Panel
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Context Provider Details"));
        
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

        enabledCheckbox = new JCheckBox("Providing Context");
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        headerPanel.add(enabledCheckbox, gbc);

        add(headerPanel, BorderLayout.NORTH);

        tabbedPane = new JTabbedPane();
        thisSysTab = new JPanel(new BorderLayout());
        childrenSysTab = new JPanel(new BorderLayout());
        thisRagTab = new JPanel(new BorderLayout());
        childrenRagTab = new JPanel(new BorderLayout());
        
        tabbedPane.addTab("System: This Provider", thisSysTab);
        tabbedPane.addTab("System: Children (Aggregated)", childrenSysTab);
        tabbedPane.addTab("RAG: This Provider", thisRagTab);
        tabbedPane.addTab("RAG: Children (Aggregated)", childrenRagTab);
        
        add(tabbedPane, BorderLayout.CENTER);
    }

    public void setContextProvider(ContextProvider cp) {
        nameLabel.setText("Provider: " + cp.getName());
        descLabel.setText("<html>" + cp.getDescription().replace("\n", "<br>") + "</html>");
        
        for (java.awt.event.ActionListener al : enabledCheckbox.getActionListeners()) {
            enabledCheckbox.removeActionListener(al);
        }
        
        enabledCheckbox.setSelected(cp.isProviding());
        enabledCheckbox.addActionListener(e -> {
            cp.setProviding(enabledCheckbox.isSelected());
            parentPanel.refresh();
            updatePreviews(cp);
        });

        updatePreviews(cp);
        revalidate();
        repaint();
    }

    private void updatePreviews(ContextProvider cp) {
        Chat chat = parentPanel.getChat();
        
        // 1. System: This Provider
        UserMessage thisSysMsg = new UserMessage(chat);
        try {
            List<String> instructions = cp.getSystemInstructions();
            if (!instructions.isEmpty()) {
                new TextPart(thisSysMsg, cp.getHeader());
                for (String part : instructions) {
                    new TextPart(thisSysMsg, part);
                }
            }
        } catch (Exception e) {
            new TextPart(thisSysMsg, "**Error generating system instructions:**\n" + ExceptionUtils.getStackTrace(e));
        }
        renderPreview(thisSysMsg, thisSysTab, "*No system instructions contributed by this provider.*");

        // 2. System: Children (Aggregated)
        UserMessage childrenSysMsg = new UserMessage(chat);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    List<String> instructions = p.getSystemInstructions();
                    if (!instructions.isEmpty()) {
                        new TextPart(childrenSysMsg, p.getHeader());
                        for (String part : instructions) {
                            new TextPart(childrenSysMsg, part);
                        }
                    }
                } catch (Exception e) {
                    new TextPart(childrenSysMsg, "**Error generating system instructions for child:**\n" + ExceptionUtils.getStackTrace(e));
                }
            }
        }
        renderPreview(childrenSysMsg, childrenSysTab, "*No system instructions contributed by children.*");

        // 3. RAG: This Provider
        RagMessage thisRagMsg = new RagMessage(chat);
        try {
            thisRagMsg.addPart(cp.getHeader());
            cp.populateMessage(thisRagMsg);
        } catch (Exception e) {
            new TextPart(thisRagMsg, "**Error populating RAG message:**\n" + ExceptionUtils.getStackTrace(e));
        }
        renderPreview(thisRagMsg, thisRagTab, "*No RAG content contributed by this provider.*");

        // 4. RAG: Children (Aggregated)
        RagMessage childrenRagMsg = new RagMessage(chat);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    childrenRagMsg.addPart(p.getHeader());
                    p.populateMessage(childrenRagMsg);
                } catch (Exception e) {
                    new TextPart(childrenRagMsg, "**Error populating RAG message for child:**\n" + ExceptionUtils.getStackTrace(e));
                }
            }
        }
        renderPreview(childrenRagMsg, childrenRagTab, "*No RAG content contributed by children.*");
    }

    private void renderPreview(UserMessage msg, JPanel tab, String emptyText) {
        if (msg.getParts().isEmpty()) {
            new TextPart(msg, emptyText);
        }
        OtherMessagePanel panel = new OtherMessagePanel(parentPanel.getChatPanel(), msg, false, false);
        panel.render();
        tab.removeAll();
        tab.add(panel, BorderLayout.CENTER);
    }
}
