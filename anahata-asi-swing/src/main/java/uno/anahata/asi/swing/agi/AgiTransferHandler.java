/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Graphics2D;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.InputEvent;
import java.io.File;
import java.nio.file.Path;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.TransferHandler;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.UICapture;

/**
 * A TransferHandler for Anahata ASI panels that automatically registers dropped 
 * files as managed V2 resources in the current agi session.
 * 
 * @author anahata
 */
@Slf4j
public class AgiTransferHandler extends TransferHandler {

    /** The parent agi panel to provide context for resource registration. */
    private final AgiPanel agiPanel;
    /** An optional delegate TransferHandler to preserve existing transfer behavior. */
    private final TransferHandler delegate;

    /**
     * Constructs a new AgiTransferHandler without a delegate.
     * 
     * @param agiPanel The parent agi panel.
     */
    public AgiTransferHandler(AgiPanel agiPanel) {
        this(agiPanel, null);
    }

    /**
     * Constructs a new AgiTransferHandler with an optional delegate.
     * 
     * @param agiPanel The parent agi panel.
     * @param delegate The delegate TransferHandler, can be null.
     */
    public AgiTransferHandler(AgiPanel agiPanel, TransferHandler delegate) {
        this.agiPanel = agiPanel;
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     * <p>Returns true if the transfer contains a list of files or if the delegate 
     * supports the import.</p>
     */
    @Override
    public boolean canImport(TransferSupport support) {
        return support.isDataFlavorSupported(DataFlavor.javaFileListFlavor) || 
               support.isDataFlavorSupported(DataFlavor.imageFlavor) ||
               (delegate != null && delegate.canImport(support));
    }

    /**
     * {@inheritDoc}
     * <p>Extracts file lists from the transfer and registers them as resources 
     * in the current session via a background task. If the data is not a file list, 
     * it falls back to the delegate.</p>
     */
    @Override
    public boolean importData(TransferSupport support) {
        if (support.isDataFlavorSupported(DataFlavor.imageFlavor)) {
            try {
                Transferable t = support.getTransferable();
                Image img = (Image) t.getTransferData(DataFlavor.imageFlavor);
                
                if (img != null) {
                    new SwingTask<>(agiPanel, "Paste Image", () -> {
                        BufferedImage bi;
                        if (img instanceof BufferedImage buff) {
                            bi = buff;
                        } else {
                            bi = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
                            Graphics2D g = bi.createGraphics();
                            g.drawImage(img, 0, 0, null);
                            g.dispose();
                        }
                        
                        String timestamp = UICapture.TIMESTAMP_FORMAT.format(new Date());
                        String filename = "pasted-image-" + timestamp + ".png";
                        Path file = UICapture.SCREENSHOTS_DIR.resolve(filename);
                        File ioFile = file.toFile();
                        ioFile.deleteOnExit();
                        ImageIO.write(bi, "png", ioFile);
                        
                        agiPanel.getInputPanel().attach(file);
                        return null;
                    }).start();
                    return true;
                }
            } catch (Exception e) {
                log.error("Failed to import pasted image", e);
            }
        }

        if (support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
            try {
                Transferable t = support.getTransferable();
                @SuppressWarnings("unchecked")
                List<File> files = (List<File>) t.getTransferData(DataFlavor.javaFileListFlavor);
                
                if (files != null && !files.isEmpty()) {
                    List<Path> paths = files.stream().map(File::toPath).collect(Collectors.toList());
                    
                    new SwingTask<>(agiPanel, "Drop Files", () -> {
                        agiPanel.getInputPanel().registerPathsAsResources(paths, "dropped into the chat by user via drag and drop");
                        return null;
                    }).start();

                    return true;
                }
            } catch (Exception e) {
                log.error("Failed to import dropped files", e);
            }
        }

        if (delegate != null) {
            return delegate.importData(support);
        }
        
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Returns the source actions supported by the delegate or defaults to COPY.</p>
     */
    @Override
    public int getSourceActions(JComponent c) {
        return delegate != null ? delegate.getSourceActions(c) : COPY;
    }

    /**
     * {@inheritDoc}
     * <p>Exports the data to the clipboard using the delegate if present, or 
     * the standard implementation.</p>
     */
    @Override
    public void exportToClipboard(JComponent comp, Clipboard clip, int action) throws IllegalStateException {
        if (delegate != null) {
            delegate.exportToClipboard(comp, clip, action);
        } else {
            super.exportToClipboard(comp, clip, action);
        }
    }

    /**
     * {@inheritDoc}
     * <p>Initiates a drag operation using the delegate if present, or the 
     * standard implementation.</p>
     */
    @Override
    public void exportAsDrag(JComponent comp, InputEvent e, int action) {
        if (delegate != null) {
            delegate.exportAsDrag(comp, e, action);
        } else {
            super.exportAsDrag(comp, e, action);
        }
    }
    
    /**
     * {@inheritDoc}
     * <p>Provides a visual representation of the transfer using the delegate 
     * if present.</p>
     */
    @Override
    public Icon getVisualRepresentation(Transferable t) {
        return delegate != null ? delegate.getVisualRepresentation(t) : super.getVisualRepresentation(t);
    }
}
