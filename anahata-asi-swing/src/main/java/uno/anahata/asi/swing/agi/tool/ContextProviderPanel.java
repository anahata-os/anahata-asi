/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
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
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.swing.agi.ContextPanel;
import uno.anahata.asi.swing.agi.render.RagMessageViewer;

/**
 * A panel that displays the details and context previews for a {@link ContextProvider}.
 * <p>
 * It dynamically manages its tabs, hiding those that have no content to show 
 * (e.g., if a provider doesn't contribute system instructions or RAG content).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ContextProviderPanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JCheckBox providingCheckbox;
    private final JLabel effectivelyProvidingLabel;
    
    private final JTabbedPane tabbedPane;
    private final JPanel thisSysTab;
    private final JPanel childrenSysTab;
    private final JPanel thisRagTab;
    private final JPanel childrenRagTab;

    /**
     * Constructs a new ContextProviderPanel.
     * @param parentPanel The parent context panel.
     */
    public ContextProviderPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        
        // Ensure the panel can be resized small enough to not squeeze the tree
        setMinimumSize(new Dimension(0, 0));

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

        providingCheckbox = new JCheckBox("Providing Context");
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        headerPanel.add(providingCheckbox, gbc);
        gbc.gridy++;
        
        effectivelyProvidingLabel = new JLabel();
        effectivelyProvidingLabel.setFont(effectivelyProvidingLabel.getFont().deriveFont(java.awt.Font.ITALIC));
        headerPanel.add(effectivelyProvidingLabel, gbc);

        add(headerPanel, BorderLayout.NORTH);

        tabbedPane = new JTabbedPane();
        thisSysTab = new JPanel(new BorderLayout());
        childrenSysTab = new JPanel(new BorderLayout());
        thisRagTab = new JPanel(new BorderLayout());
        childrenRagTab = new JPanel(new BorderLayout());
        
        add(tabbedPane, BorderLayout.CENTER);
    }

    /**
     * Sets the context provider to display and updates the UI.
     * @param cp The context provider.
     */
    public void setContextProvider(ContextProvider cp) {
        nameLabel.setText("Provider: " + cp.getName());
        descLabel.setText("<html>" + cp.getDescription().replace("\n", "<br>") + "</html>");
        
        for (java.awt.event.ActionListener al : providingCheckbox.getActionListeners()) {
            providingCheckbox.removeActionListener(al);
        }
        
        providingCheckbox.setSelected(cp.isProviding());
        providingCheckbox.addActionListener(e -> {
            cp.setProviding(providingCheckbox.isSelected());
            // Use non-structural refresh to preserve selection and expansion
            parentPanel.refresh(false);
            updatePreviews(cp);
            updateEffectivelyProviding(cp);
        });

        updateEffectivelyProviding(cp);
        updatePreviews(cp);
        revalidate();
        repaint();
    }
    
    /**
     * Updates the 'Effectively Providing' label based on the provider's hierarchy.
     */
    private void updateEffectivelyProviding(ContextProvider cp) {
        if (cp.getParentProvider() != null) {
            effectivelyProvidingLabel.setVisible(true);
            boolean effective = cp.isEffectivelyProviding();
            effectivelyProvidingLabel.setText("Effectively Providing: " + (effective ? "Yes" : "No"));
            effectivelyProvidingLabel.setForeground(effective ? new Color(0, 128, 0) : Color.RED);
        } else {
            effectivelyProvidingLabel.setVisible(false);
        }
    }

    /**
     * Updates the content previews and manages tab visibility.
     */
    private void updatePreviews(ContextProvider cp) {
        Agi agi = parentPanel.getAgi();
        tabbedPane.removeAll();
        
        // 1. System: This Provider
        RagMessage thisSysMsg = new RagMessage(agi);
        try {
            List<String> instructions = cp.getSystemInstructions();
            if (!instructions.isEmpty()) {
                thisSysMsg.addTextPart(cp.getHeader());
                for (String part : instructions) {
                    thisSysMsg.addTextPart(part);
                }
            }
        } catch (Exception e) {
            thisSysMsg.addTextPart("**Error generating system instructions:**\n" + ExceptionUtils.getStackTrace(e));
        }
        if (!thisSysMsg.getParts().isEmpty()) {
            renderPreview(thisSysMsg, thisSysTab, "");
            tabbedPane.addTab("System: This Provider", thisSysTab);
        }

        // 2. System: Children (Aggregated)
        RagMessage childrenSysMsg = new RagMessage(agi);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    List<String> instructions = p.getSystemInstructions();
                    if (!instructions.isEmpty()) {
                        childrenSysMsg.addTextPart(p.getHeader());
                        for (String part : instructions) {
                            childrenSysMsg.addTextPart(part);
                        }
                    }
                } catch (Exception e) {
                    childrenSysMsg.addTextPart("**Error generating system instructions for child:**\n" + ExceptionUtils.getStackTrace(e));
                }
            }
        }
        if (!childrenSysMsg.getParts().isEmpty()) {
            renderPreview(childrenSysMsg, childrenSysTab, "");
            tabbedPane.addTab("System: Children (Aggregated)", childrenSysTab);
        }

        // 3. RAG: This Provider
        RagMessage thisRagMsg = new RagMessage(agi);
        try {
            cp.populateMessage(thisRagMsg);
        } catch (Exception e) {
            thisRagMsg.addTextPart("**Error populating RAG message:**\n" + ExceptionUtils.getStackTrace(e));
        }
        if (!thisRagMsg.getParts().isEmpty()) {
            renderPreview(thisRagMsg, thisRagTab, "");
            tabbedPane.addTab("RAG: This Provider", thisRagTab);
        }

        // 4. RAG: Children (Aggregated)
        RagMessage childrenRagMsg = new RagMessage(agi);
        for (ContextProvider child : cp.getChildrenProviders()) {
            for (ContextProvider p : child.getFlattenedHierarchy(true)) {
                try {
                    p.populateMessage(childrenRagMsg);
                } catch (Exception e) {
                    childrenRagMsg.addTextPart("**Error populating RAG message for child:**\n" + ExceptionUtils.getStackTrace(e));
                }
            }
        }
        if (!childrenRagMsg.getParts().isEmpty()) {
            renderPreview(childrenRagMsg, childrenRagTab, "");
            tabbedPane.addTab("RAG: Children (Aggregated)", childrenRagTab);
        }
        
        if (tabbedPane.getTabCount() == 0) {
            JPanel emptyPanel = new JPanel(new BorderLayout());
            emptyPanel.add(new JLabel("No context content contributed by this provider.", SwingConstants.CENTER));
            tabbedPane.addTab("No Content", emptyPanel);
        }
    }

    private void renderPreview(RagMessage msg, JPanel tab, String emptyText) {
        if (msg.getParts().isEmpty() && !emptyText.isEmpty()) {
            msg.addTextPart(emptyText);
        }
        RagMessageViewer panel = new RagMessageViewer(parentPanel.getAgiPanel(), msg, false, false);
        panel.render();
        tab.removeAll();
        tab.add(panel, BorderLayout.CENTER);
    }
}
