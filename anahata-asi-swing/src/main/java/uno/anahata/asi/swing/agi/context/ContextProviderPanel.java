/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

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
import uno.anahata.asi.swing.components.ScrollablePanel;
import javax.swing.SwingConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.swing.agi.context.ContextPanel;
import uno.anahata.asi.swing.agi.message.RagMessageViewer;
import uno.anahata.asi.swing.internal.SwingTask;

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
public class ContextProviderPanel extends ScrollablePanel {

    /**
     * The parent context management panel.
     */
    private final ContextPanel parentPanel;
    
    /**
     * Label for the provider's friendly name.
     */
    private final JLabel nameLabel;
    /**
     * Label for the provider's description.
     */
    private final JLabel descLabel;
    /**
     * Checkbox to toggle providing status.
     */
    private final JCheckBox providingCheckbox;
    /**
     * Label indicating if the provider is effectively providing context.
     */
    private final JLabel effectivelyProvidingLabel;
    
    /**
     * Tabbed pane containing previews.
     */
    private final JTabbedPane tabbedPane;
    /**
     * Tab for system instructions from this provider.
     */
    private final JPanel thisSysTab;
    /**
     * Tab for aggregated system instructions from children.
     */
    private final JPanel childrenSysTab;
    /**
     * Tab for RAG content from this provider.
     */
    private final JPanel thisRagTab;
    /**
     * Tab for aggregated RAG content from children.
     */
    private final JPanel childrenRagTab;
    
    /**
     * The context provider currently being inspected.
     */
    private ContextProvider currentProvider;

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
        this.currentProvider = cp;
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
     * @param cp The current context provider.
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
     * Updates the content previews and manages tab visibility asynchronously.
     * @param cp The current context provider.
     */
    private void updatePreviews(ContextProvider cp) {
        Agi agi = parentPanel.getAgi();
        tabbedPane.removeAll();
        
        JPanel loadingPanel = new JPanel(new BorderLayout());
        loadingPanel.add(new JLabel("Sensing Provider Content...", SwingConstants.CENTER), BorderLayout.CENTER);
        tabbedPane.addTab("Loading...", loadingPanel);

        new SwingTask<RagMessage[]>(parentPanel.getAgiPanel(), "Loading Previews for " + cp.getName(), () -> {
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

            // 3. RAG: This Provider
            RagMessage thisRagMsg = new RagMessage(agi);
            try {
                cp.populateMessage(thisRagMsg);
            } catch (Exception e) {
                thisRagMsg.addTextPart("**Error populating RAG message:**\n" + ExceptionUtils.getStackTrace(e));
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
            
            return new RagMessage[] { thisSysMsg, childrenSysMsg, thisRagMsg, childrenRagMsg };
        }, msgs -> {
            // Abort UI update if the user clicked away during load
            if (this.currentProvider != cp) {
                return;
            }
            
            tabbedPane.removeAll();
            
            RagMessage thisSysMsg = msgs[0];
            if (!thisSysMsg.getParts().isEmpty()) {
                renderPreview(thisSysMsg, thisSysTab, "");
                tabbedPane.addTab("System: This Provider", thisSysTab);
            }

            RagMessage childrenSysMsg = msgs[1];
            if (!childrenSysMsg.getParts().isEmpty()) {
                renderPreview(childrenSysMsg, childrenSysTab, "");
                tabbedPane.addTab("System: Children (Aggregated)", childrenSysTab);
            }

            RagMessage thisRagMsg = msgs[2];
            if (!thisRagMsg.getParts().isEmpty()) {
                renderPreview(thisRagMsg, thisRagTab, "");
                tabbedPane.addTab("RAG: This Provider", thisRagTab);
            }

            RagMessage childrenRagMsg = msgs[3];
            if (!childrenRagMsg.getParts().isEmpty()) {
                renderPreview(childrenRagMsg, childrenRagTab, "");
                tabbedPane.addTab("RAG: Children (Aggregated)", childrenRagTab);
            }
            
            if (tabbedPane.getTabCount() == 0) {
                JPanel emptyPanel = new JPanel(new BorderLayout());
                emptyPanel.add(new JLabel("No context content contributed by this provider.", SwingConstants.CENTER));
                tabbedPane.addTab("No Content", emptyPanel);
            }
        }).start();
    }

    /**
     * Surgically renders the preview message inside the target tab panel.
     * @param tab The tab panel to render into.
     * @param msg The message to render.
     * @param emptyText Fallback text if the message is empty.
     */
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
