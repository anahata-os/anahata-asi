/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.InputEvent;
import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.TransferHandler;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.internal.SwingTask;

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
                    }).execute();

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
