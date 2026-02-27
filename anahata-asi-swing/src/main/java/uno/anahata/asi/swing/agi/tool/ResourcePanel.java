/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.resource.RefreshPolicy;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.swing.agi.ContextPanel;
import uno.anahata.asi.swing.agi.render.RagMessageViewer;

/**
 * A panel that displays the details and a content preview for an {@link AbstractResource}.
 * <p>
 * It provides controls to modify resource metadata (Position, Policy, Providing) 
 * and integrated IDE actions like "Open in Editor".
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ResourcePanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JTextField uuidField;
    private final JCheckBox providingBox;
    private final JComboBox<ContextPosition> positionCombo;
    private final JComboBox<RefreshPolicy> policyCombo;
    private final JLabel viewportLabel;
    
    private final JTabbedPane contentTabs;
    private AbstractResource<?, ?> currentResource;

    /**
     * Constructs a new ResourcePanel.
     * @param parentPanel The parent context panel.
     */
    public ResourcePanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        setMinimumSize(new Dimension(0, 0));

        // Header Panel: Resource Properties
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Resource Properties"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 5, 2, 5);

        // Name and IDE Actions
        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(java.awt.Font.BOLD, 14f));
        headerPanel.add(nameLabel, gbc);
        
        gbc.gridx = 1;
        gbc.weightx = 1.0;
        JPanel actionPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        actionPanel.setOpaque(false);
        
        JButton openBtn = createLinkButton("Open in Editor", "Open the original file in the IDE editor");
        openBtn.addActionListener(e -> {
            if (currentResource != null) {
                parentPanel.getAgi().getConfig().getContainer().openResource(currentResource);
            }
        });
        actionPanel.add(openBtn);

        JButton selectBtn = createLinkButton("Select in Projects", "Locate the file in the Projects tree");
        selectBtn.addActionListener(e -> {
            if (currentResource != null) {
                parentPanel.getAgi().getConfig().getContainer().selectResource(currentResource);
            }
        });
        actionPanel.add(selectBtn);

        headerPanel.add(actionPanel, gbc);

        // UUID (Read-only)
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        headerPanel.add(new JLabel("UUID:"), gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        uuidField = new JTextField();
        uuidField.setEditable(false);
        uuidField.setBorder(null);
        uuidField.setOpaque(false);
        headerPanel.add(uuidField, gbc);

        // Providing Checkbox
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.fill = GridBagConstraints.NONE;
        providingBox = new JCheckBox("Providing Context");
        providingBox.addActionListener(e -> {
            if (currentResource != null) currentResource.setProviding(providingBox.isSelected());
        });
        headerPanel.add(providingBox, gbc);

        // Position and Policy
        JPanel comboPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
        comboPanel.setOpaque(false);
        
        comboPanel.add(new JLabel("Position:"));
        positionCombo = new JComboBox<>(ContextPosition.values());
        positionCombo.addActionListener(e -> {
            if (currentResource != null) currentResource.setContextPosition((ContextPosition) positionCombo.getSelectedItem());
        });
        comboPanel.add(positionCombo);
        
        comboPanel.add(new JLabel("Refresh Policy:"));
        policyCombo = new JComboBox<>(RefreshPolicy.values());
        policyCombo.addActionListener(e -> {
            if (currentResource != null) currentResource.setRefreshPolicy((RefreshPolicy) policyCombo.getSelectedItem());
        });
        comboPanel.add(policyCombo);
        
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        headerPanel.add(comboPanel, gbc);

        // Viewport Settings Summary
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 2;
        viewportLabel = new JLabel();
        viewportLabel.setFont(viewportLabel.getFont().deriveFont(java.awt.Font.ITALIC));
        headerPanel.add(viewportLabel, gbc);

        add(headerPanel, BorderLayout.NORTH);

        // Content Tabs
        contentTabs = new JTabbedPane();
        add(contentTabs, BorderLayout.CENTER);
    }

    private JButton createLinkButton(String text, String tooltip) {
        JButton btn = new JButton("<html><a href='#'>" + text + "</a></html>");
        btn.setToolTipText(tooltip);
        btn.setBorderPainted(false);
        btn.setOpaque(false);
        btn.setBackground(new Color(0, 0, 0, 0));
        btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return btn;
    }

    /**
     * Sets the resource to display and updates the UI.
     * @param res The resource.
     */
    public void setResource(AbstractResource<?, ?> res) {
        this.currentResource = res;
        nameLabel.setText(res.getName());
        uuidField.setText(res.getId());
        providingBox.setSelected(res.isProviding());
        positionCombo.setSelectedItem(res.getContextPosition());
        policyCombo.setSelectedItem(res.getRefreshPolicy());
        
        if (res instanceof TextFileResource tfr) {
            viewportLabel.setText("Viewport: " + tfr.getViewport().getSettings().toString());
        } else {
            viewportLabel.setText("");
        }
        
        updateTabs(res);
        revalidate();
        repaint();
    }

    private void updateTabs(AbstractResource<?, ?> res) {
        contentTabs.removeAll();
        
        Agi agi = parentPanel.getAgi();
        
        // Tab 1: Raw view (Definitive Model Perspective)
        RagMessage rawMsg = new RagMessage(agi);
        try {
            // Explicitly add the header as its own part to mirror the RAG assembly logic
            rawMsg.addTextPart(res.getHeader());
            
            if (res.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                res.getSystemInstructions().forEach(rawMsg::addTextPart);
            } else {
                res.populateMessage(rawMsg);
            }
        } catch (Exception e) {
            rawMsg.addTextPart("**Error generating raw preview:**\n" + ExceptionUtils.getStackTrace(e));
        }
        
        RagMessageViewer rawViewer = new RagMessageViewer(parentPanel.getAgiPanel(), rawMsg, false, false);
        rawViewer.render();
        contentTabs.addTab("Raw (Model View)", rawViewer);
        
        // Tab 2: Full Preview (User/IDE Perspective)
        if (res instanceof TextFileResource tfr) {
            RagMessage fullMsg = new RagMessage(agi);
            try {
                // For the full preview, we don't show the header, just the content
                fullMsg.addTextPart(tfr.getContent());
            } catch (Exception ex) {
                fullMsg.addTextPart("Error loading full content: " + ex.getMessage());
            }
            RagMessageViewer fullViewer = new RagMessageViewer(parentPanel.getAgiPanel(), fullMsg, false, false);
            fullViewer.render();
            contentTabs.addTab("Full Preview", fullViewer);
        }
    }
}
