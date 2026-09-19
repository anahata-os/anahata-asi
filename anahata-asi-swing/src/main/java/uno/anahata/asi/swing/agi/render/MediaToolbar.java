/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import lombok.NonNull;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.icons.NextIcon;
import uno.anahata.asi.swing.icons.SaveIcon;

/**
 * Standardized action and metadata toolbar for multimodal media viewers.
 * <p>
 * Provides universal operations across all media presentations:
 * </p>
 * <ul>
 *   <li><b>Copy:</b> Places decoded image pixels or source file onto the system clipboard.</li>
 *   <li><b>Save As...:</b> Exports the media stream directly to disk via {@link JFileChooser}.</li>
 *   <li><b>Open External:</b> Dispatches to the OS default player/viewer via {@link Desktop#open(File)}.</li>
 *   <li><b>Open in IDE:</b> Dispatches to the host IDE editor or native image viewer.</li>
 * </ul>
 * 
 * @author anahata
 */
@Slf4j
public class MediaToolbar extends JPanel {

    /** The parent AgiPanel providing the session context and config. */
    private final AgiPanel agiPanel;

    /** Label displaying format, dimensions, and file size. */
    private final JLabel badgeLabel = new JLabel();

    /** Button for copying media to the system clipboard. */
    private final JButton copyButton;

    /** Button for saving the media to disk. */
    private final JButton saveButton;

    /** Button for opening the media in an external application. */
    private final JButton externalButton;

    /** Button for opening the media inside the host IDE. */
    private final JButton ideButton;

    /** The raw binary data. */
    @Getter @Setter
    private byte[] data;

    /** The MIME type string. */
    @Getter @Setter
    private String mimeType;

    /** The display name or file name. */
    @Getter @Setter
    private String displayName;

    /** The source URI, if available. */
    @Getter @Setter
    private URI sourceUri;

    /** Optional supplier for the current image pixels, used for clipboard copy. */
    @Setter
    private java.util.function.Supplier<BufferedImage> imageSupplier;

    /**
     * Constructs a new MediaToolbar with standard action buttons and badge.
     *
     * @param agiPanel The parent AgiPanel providing configuration and session context.
     */
    public MediaToolbar(@NonNull AgiPanel agiPanel) {
        super(new BorderLayout());
        this.agiPanel = agiPanel;
        setOpaque(false);

        SwingAgiConfig config = agiPanel.getAgiConfig();

        badgeLabel.setFont(new Font("SansSerif", Font.BOLD, 10));
        badgeLabel.setForeground(new Color(140, 150, 165));

        JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        buttonsPanel.setOpaque(false);

        copyButton = config.createSquareButton(ActionIconKey.COPY, 14, "Copy Media to System Clipboard");
        copyButton.setFocusable(false);
        copyButton.addActionListener(e -> copyToClipboard());

        saveButton = config.createSquareButton(ActionIconKey.SAVE, 14, "Save Media to File...");
        saveButton.setFocusable(false);
        saveButton.addActionListener(e -> saveAs());

        externalButton = config.createSquareButton(ActionIconKey.EXTERNAL, 14, "Open in System Default Application (VLC, Image Viewer, etc.)");
        externalButton.setFocusable(false);
        externalButton.addActionListener(e -> openExternal());

        ideButton = config.createSquareButton(ActionIconKey.OPEN_IN_IDE, 14, "Open in Host IDE");
        ideButton.setFocusable(false);
        ideButton.addActionListener(e -> openInIde());

        buttonsPanel.add(copyButton);
        buttonsPanel.add(saveButton);
        buttonsPanel.add(externalButton);
        buttonsPanel.add(ideButton);

        add(badgeLabel, BorderLayout.WEST);
        add(buttonsPanel, BorderLayout.EAST);
    }

    /**
     * Updates the badge and button visibility based on the media metadata.
     *
     * @param extraDetails Optional details to append to the badge (e.g. image dimensions).
     */
    public void updateMetadata(String extraDetails) {
        StringBuilder sb = new StringBuilder();
        if (mimeType != null) {
            String shortType = mimeType.substring(mimeType.indexOf('/') + 1).toUpperCase();
            sb.append(shortType);
        }
        if (extraDetails != null && !extraDetails.isBlank()) {
            if (sb.length() > 0) sb.append(" • ");
            sb.append(extraDetails);
        }
        if (data != null && data.length > 0) {
            if (sb.length() > 0) sb.append(" • ");
            sb.append(TextUtils.formatSize(data.length));
        }
        badgeLabel.setText(sb.toString());

        // IDE button only visible if we have a real file URI
        boolean isFileUri = sourceUri != null && "file".equalsIgnoreCase(sourceUri.getScheme());
        ideButton.setVisible(isFileUri);
    }

    /**
     * Copies the media content to the system clipboard.
     */
    public void copyToClipboard() {
        if (imageSupplier != null) {
            BufferedImage img = imageSupplier.get();
            if (img != null) {
                SwingUtils.copyImageToClipboard(img);
                log.info("Copied image pixels to system clipboard.");
                return;
            }
        }

        // File-based copy
        File targetFile = resolveLocalFile();
        if (targetFile != null && targetFile.exists()) {
            SwingUtils.copyFilesToClipboard(List.of(targetFile));
            log.info("Copied file to system clipboard: {}", targetFile);
        }
    }

    /**
     * Opens a JFileChooser dialog to save the media to disk.
     */
    public void saveAs() {
        if (data == null || data.length == 0) {
            JOptionPane.showMessageDialog(this, "No media data available to save.", "Save Error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        JFileChooser chooser = new JFileChooser();
        String ext = TikaUtils.getExtension(mimeType);
        String defaultName = (displayName != null && !displayName.isBlank()) ? displayName : "media" + ext;
        if (!defaultName.contains(".")) {
            defaultName += ext;
        }
        chooser.setSelectedFile(new File(defaultName));

        if (chooser.showSaveDialog(SwingUtilities.getWindowAncestor(this)) == JFileChooser.APPROVE_OPTION) {
            File dest = chooser.getSelectedFile();
            try {
                Files.write(dest.toPath(), data);
                log.info("Media saved successfully to: {}", dest);
                JOptionPane.showMessageDialog(this, "File saved successfully:\n" + dest.getAbsolutePath(), "Save Media", JOptionPane.INFORMATION_MESSAGE);
            } catch (IOException ex) {
                log.error("Failed to save media file", ex);
                JOptionPane.showMessageDialog(this, "Failed to save file: " + ex.getMessage(), "Save Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * Opens the media file in the OS default application.
     */
    public void openExternal() {
        File file = resolveLocalFile();
        if (file != null && file.exists()) {
            try {
                Desktop.getDesktop().open(file);
            } catch (Exception ex) {
                log.error("Failed to open external file: {}", file, ex);
                JOptionPane.showMessageDialog(this, "Failed to open in external application: " + ex.getMessage(), "Launch Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    /**
     * Opens the media in the host IDE editor or viewer.
     */
    public void openInIde() {
        if (sourceUri != null) {
            ResourceUiRegistry.getInstance().getResourceUI().openUri(sourceUri.toString());
        }
    }

    /**
     * Resolves an existing local file or writes data to a temporary file for external access.
     */
    private File resolveLocalFile() {
        if (sourceUri != null && "file".equalsIgnoreCase(sourceUri.getScheme())) {
            File f = new File(sourceUri);
            if (f.exists()) {
                return f;
            }
        }

        if (data != null && data.length > 0) {
            try {
                String ext = TikaUtils.getExtension(mimeType);
                String prefix = displayName != null ? displayName.replaceAll("[^a-zA-Z0-9.-]", "_") + "_" : "media_";
                if (prefix.length() < 3) prefix = "media_";
                File temp = File.createTempFile(prefix, ext);
                temp.deleteOnExit();
                try (FileOutputStream fos = new FileOutputStream(temp)) {
                    fos.write(data);
                }
                return temp;
            } catch (IOException e) {
                log.error("Failed to write temporary media file", e);
            }
        }
        return null;
    }
}
