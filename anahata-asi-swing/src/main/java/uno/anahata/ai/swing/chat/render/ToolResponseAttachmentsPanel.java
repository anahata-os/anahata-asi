/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import uno.anahata.ai.internal.TextUtils;
import uno.anahata.ai.internal.TikaUtils;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolResponseAttachment;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.icons.DeleteIcon;
import uno.anahata.ai.swing.icons.SearchIcon;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel;

/**
 * A panel for rendering a list of {@link ToolResponseAttachment}s.
 * It uses a diff-based approach to avoid unnecessary re-renders.
 * 
 * @author anahata
 */
@Slf4j
public class ToolResponseAttachmentsPanel extends JPanel {

    private final ChatPanel chatPanel;
    private AbstractToolResponse<?> response;
    private final Map<ToolResponseAttachment, JPanel> cachedPanels = new HashMap<>();
    /** Map to track playback stoppers for audio attachments. */
    private final Map<ToolResponseAttachment, Runnable> playbackStoppers = new HashMap<>();

    public ToolResponseAttachmentsPanel(@NonNull ChatPanel chatPanel) {
        this.chatPanel = chatPanel;
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
                Runnable stopper = playbackStoppers.remove(attachment);
                if (stopper != null) stopper.run();
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

    private JPanel createAttachmentPanel(ToolResponseAttachment attachment) {
        JPanel itemPanel = new JPanel(new MigLayout("fillx, insets 5, gap 0", "[grow]", "[]0[]"));
        itemPanel.setOpaque(false);
        itemPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, java.awt.Color.LIGHT_GRAY));

        String mimeType = attachment.getMimeType();
        byte[] data = attachment.getData();

        // Label and Info
        JLabel infoLabel = new JLabel("Attachment (" + mimeType + "): " + TextUtils.formatSize(data.length));
        
        JButton viewButton = new JButton(new SearchIcon(14));
        viewButton.setToolTipText("View Attachment");
        viewButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
        viewButton.setFocusable(false);
        viewButton.addActionListener(e -> viewAttachment(attachment));

        JButton deleteButton = new JButton(new DeleteIcon(14));
        deleteButton.setToolTipText("Remove Attachment");
        deleteButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
        deleteButton.setFocusable(false);
        deleteButton.addActionListener(e -> response.removeAttachment(attachment));

        // Add them to the first row: Label and View on the left, Delete on the far right
        itemPanel.add(infoLabel, "split 3, gapright 0");
        itemPanel.add(viewButton, "gapright push");
        itemPanel.add(deleteButton, "right, wrap");

        // Media Content (Image/Audio) below the label
        if (mimeType.startsWith("image/")) {
            itemPanel.add(MediaRenderer.createImageComponent(data, this), "growx, wrap");
        } else if (mimeType.startsWith("audio/")) {
            AudioPlaybackPanel audioPanel = chatPanel.getStatusPanel().getAudioPlaybackPanel();
            itemPanel.add(MediaRenderer.createAudioComponent(data, audioPanel, stopper -> {
                if (stopper == null) {
                    Runnable oldStopper = playbackStoppers.remove(attachment);
                    if (oldStopper != null) oldStopper.run();
                } else {
                    playbackStoppers.put(attachment, stopper);
                }
            }), "growx, wrap");
        }

        return itemPanel;
    }

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

    @Override
    public void removeNotify() {
        playbackStoppers.values().forEach(Runnable::run);
        playbackStoppers.clear();
        super.removeNotify();
    }
}
