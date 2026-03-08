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

    private final AgiPanel agiPanel;
    private final TransferHandler delegate;

    public AgiTransferHandler(AgiPanel agiPanel) {
        this(agiPanel, null);
    }

    public AgiTransferHandler(AgiPanel agiPanel, TransferHandler delegate) {
        this.agiPanel = agiPanel;
        this.delegate = delegate;
    }

    @Override
    public boolean canImport(TransferSupport support) {
        return support.isDataFlavorSupported(DataFlavor.javaFileListFlavor) || 
               (delegate != null && delegate.canImport(support));
    }

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

    @Override
    public int getSourceActions(JComponent c) {
        return delegate != null ? delegate.getSourceActions(c) : COPY;
    }

    @Override
    public void exportToClipboard(JComponent comp, Clipboard clip, int action) throws IllegalStateException {
        if (delegate != null) {
            delegate.exportToClipboard(comp, clip, action);
        } else {
            super.exportToClipboard(comp, clip, action);
        }
    }

    @Override
    public void exportAsDrag(JComponent comp, InputEvent e, int action) {
        if (delegate != null) {
            delegate.exportAsDrag(comp, e, action);
        } else {
            super.exportAsDrag(comp, e, action);
        }
    }
    
    @Override
    public Icon getVisualRepresentation(Transferable t) {
        return delegate != null ? delegate.getVisualRepresentation(t) : super.getVisualRepresentation(t);
    }
}
