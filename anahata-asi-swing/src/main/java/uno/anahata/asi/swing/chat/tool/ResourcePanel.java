/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.chat.ContextPanel;
import uno.anahata.asi.swing.chat.render.OtherMessagePanel;

/**
 * A panel that displays the details and a content preview for an {@link AbstractResource}.
 * <p>
 * It uses an {@link OtherMessagePanel} to render the resource's content exactly 
 * as it would appear in a RAG message or system instructions, respecting 
 * viewports and binary data handling.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ResourcePanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JPanel previewContainer;

    /**
     * Constructs a new ResourcePanel.
     * @param parentPanel The parent context panel.
     */
    public ResourcePanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        
        // Ensure the panel can be resized small enough to not squeeze the tree
        setMinimumSize(new Dimension(0, 0));

        // Header Panel
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Resource Details"));
        
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

        add(headerPanel, BorderLayout.NORTH);

        // Preview Container
        previewContainer = new JPanel(new BorderLayout());
        previewContainer.setBorder(BorderFactory.createTitledBorder("Content Preview (Context View)"));
        
        add(previewContainer, BorderLayout.CENTER);
    }

    /**
     * Sets the resource to display and updates the UI.
     * @param res The resource.
     */
    public void setResource(AbstractResource<?, ?> res) {
        nameLabel.setText("Resource: " + res.getName());
        descLabel.setText("<html>" + res.getDescription().replace("\n", "<br>") + "</html>");
        
        updatePreview(res);
        revalidate();
        repaint();
    }

    /**
     * Updates the content preview by generating a synthetic message based on 
     * the resource's prompt position.
     */
    private void updatePreview(AbstractResource<?, ?> res) {
        Chat chat = parentPanel.getChat();
        AbstractMessage previewMsg;
        
        try {
            if (res.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                // For system instructions, we render the text instructions in a UserMessage
                previewMsg = new UserMessage(chat);
                List<String> instructions = res.getSystemInstructions();
                for (String inst : instructions) {
                    new TextPart(previewMsg, inst);
                }
            } else {
                // For prompt augmentation, we use the standard RAG population logic
                previewMsg = new RagMessage(chat);
                res.populateMessage((RagMessage) previewMsg);
            }
        } catch (Exception e) {
            previewMsg = new RagMessage(chat);
            new TextPart(previewMsg, "**Error generating resource preview:**\n" + ExceptionUtils.getStackTrace(e));
        }
        
        OtherMessagePanel panel = new OtherMessagePanel(parentPanel.getChatPanel(), previewMsg, false, false);
        panel.render();
        
        previewContainer.removeAll();
        previewContainer.add(panel, BorderLayout.CENTER);
    }
}
