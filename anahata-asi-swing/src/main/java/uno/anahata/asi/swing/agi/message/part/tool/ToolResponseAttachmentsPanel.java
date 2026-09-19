/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part.tool;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Insets;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.MediaRenderer;
import uno.anahata.asi.swing.agi.render.MediaViewerComponent;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.SearchIcon;

/**
 * A panel for rendering a list of {@link ToolResponseAttachment}s.
 * It uses a diff-based approach to avoid unnecessary re-renders.
 * 
 * @author anahata
 */
@Slf4j
public class ToolResponseAttachmentsPanel extends JPanel {

    /** The parent agi panel providing UI context. */
    private final AgiPanel agiPanel;
    /** The tool response containing the attachments being rendered. */
    private AbstractToolResponse<?> response;
    /** Cache of rendered panels to support incremental diff-based updates. */
    private final Map<ToolResponseAttachment, JPanel> cachedPanels = new HashMap<>();
    /** Map to track active media viewers for attachments to ensure clean disposal. */
    private final Map<ToolResponseAttachment, MediaViewerComponent> activeViewers = new HashMap<>();

    /**
     * Constructs a new ToolResponseAttachmentsPanel.
     * @param agiPanel The parent panel.
     */
    public ToolResponseAttachmentsPanel(@NonNull AgiPanel agiPanel) {
        this.agiPanel = agiPanel;
        setLayout(new MigLayout("fillx, insets 0, gap 0", "[grow]", "[]"));
        setOpaque(false);
    }

    /**
     * Updates the panel with the given list of attachments.
     * 
     * @param response The tool response containing the attachments.
     */
    public void render(AbstractToolResponse<?> response) {
        this.response = response;
        List<ToolResponseAttachment> attachments = response.getAttachments();
        
        // 1. Remove panels for attachments no longer present
        cachedPanels.keySet().removeIf(attachment -> {
            if (!attachments.contains(attachment)) {
                JPanel panel = cachedPanels.get(attachment);
                if (panel != null) {
                    remove(panel);
                }
                MediaViewerComponent viewer = activeViewers.remove(attachment);
                if (viewer != null) viewer.dispose();
                return true;
            }
            return false;
        });

        // 2. Add or update panels for current attachments
        for (int i = 0; i < attachments.size(); i++) {
            ToolResponseAttachment attachment = attachments.get(i);
            JPanel panel = cachedPanels.get(attachment);
            if (panel == null) {
                panel = createAttachmentPanel(attachment);
                cachedPanels.put(attachment, panel);
            }
            
            if (i >= getComponentCount() || getComponent(i) != panel) {
                add(panel, "growx, wrap", i);
            }
        }

        // 3. Clean up trailing components
        while (getComponentCount() > attachments.size()) {
            remove(getComponentCount() - 1);
        }

        revalidate();
        repaint();
    }

    /**
     * Creates a specialized UI panel for a single attachment.
     * <p>
     * Supports high-fidelity rendering of images and integrated audio 
     * playback observers.
     * </p>
     * @param attachment The attachment to render.
     * @return The configured JPanel.
     */
    private JPanel createAttachmentPanel(ToolResponseAttachment attachment) {
        JPanel itemPanel = new JPanel(new MigLayout("fillx, insets 5, gap 0", "[grow]", "[]0[]"));
        itemPanel.setOpaque(false);
        itemPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, agiPanel.getAgiConfig().getTheme().getChromeBorder()));

        String mimeType = attachment.getMimeType();
        byte[] data = attachment.getData();

        // Label and Info
        JLabel infoLabel = new JLabel("Attachment (" + mimeType + "): " + TextUtils.formatSize(data.length));
        
        JButton viewButton = new JButton(new SearchIcon(14));
        viewButton.setToolTipText("View Attachment");
        viewButton.setMargin(new Insets(0, 0, 0, 0));
        viewButton.setFocusable(false);
        viewButton.addActionListener(e -> viewAttachment(attachment));

        JButton deleteButton = new JButton(new DeleteIcon(14));
        deleteButton.setToolTipText("Remove Attachment");
        deleteButton.setMargin(new Insets(0, 0, 0, 0));
        deleteButton.setFocusable(false);
        deleteButton.addActionListener(e -> response.removeAttachment(attachment));

        // Add them to the first row: Label and View on the left, Delete on the far right
        itemPanel.add(infoLabel, "split 3, gapright 0");
        itemPanel.add(viewButton, "gapright push");
        itemPanel.add(deleteButton, "right, wrap");

        // Unified multimodal viewer (Image, Video, Audio, or File card)
        AbstractSwingAsiContainer container = (AbstractSwingAsiContainer) agiPanel.getAgi().getConfig().getAsiContainer();
        MediaViewerComponent viewer = MediaRenderer.createViewer(data, mimeType, "attachment", null, container, agiPanel);
        activeViewers.put(attachment, viewer);
        itemPanel.add(viewer.getComponent(), "growx, wrap");

        return itemPanel;
    }

    /**
     * Dispatches an attachment to the host-native viewing application.
     * <p>
     * Creates a temporary file based on the detected MIME type and uses 
     * {@link Desktop#open(File)} for high-fidelity inspection.
     * </p>
     * @param attachment The attachment to view.
     */
    private void viewAttachment(ToolResponseAttachment attachment) {
        try {
            String extension = TikaUtils.getExtension(attachment.getMimeType());
            File tempFile = File.createTempFile("attachment-", extension);
            tempFile.deleteOnExit();
            try (FileOutputStream fos = new FileOutputStream(tempFile)) {
                fos.write(attachment.getData());
            }
            Desktop.getDesktop().open(tempFile);
        } catch (Exception e) {
            log.error("Failed to view attachment", e);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Ensures all active media viewers 
     * are stopped when the component is removed from the UI hierarchy.</p>
     */
    @Override
    public void removeNotify() {
        activeViewers.values().forEach(MediaViewerComponent::stop);
        super.removeNotify();
    }
}
