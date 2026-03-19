/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.internal;

import uno.anahata.asi.swing.components.ExceptionDialog;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.MouseWheelEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import javax.swing.JComponent;
import javax.swing.JPanel;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.text.CodeBlockSegmentRenderer;
import uno.anahata.asi.swing.agi.message.part.text.MermaidCodeBlockSegmentRenderer;

/**
 * A collection of general-purpose Swing utility methods, primarily for image
 * manipulation and UI component creation.
 * <p>
 * This utility provides high-fidelity component discovery and specialized 
 * event redispatching to ensure a seamless interaction between nested 
 * host-assembled frames and the core conversation UI.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@UtilityClass
public class SwingUtils {

    /** The maximum dimension for generated thumbnails. */
    private static final int THUMBNAIL_MAX_SIZE = 250;

    /**
     * Creates a scaled thumbnail image from an original BufferedImage, maintaining
     * the aspect ratio and ensuring the largest dimension does not exceed
     * {@code THUMBNAIL_MAX_SIZE}.
     *
     * @param original The original image.
     * @return The scaled thumbnail image.
     */
    public static Image createThumbnail(BufferedImage original) {
        int width = original.getWidth();
        int height = original.getHeight();

        if (width <= THUMBNAIL_MAX_SIZE && height <= THUMBNAIL_MAX_SIZE) {
            return original;
        }

        double thumbRatio = (double) THUMBNAIL_MAX_SIZE / (double) Math.max(width, height);
        int newWidth = (int) (width * thumbRatio);
        int newHeight = (int) (height * thumbRatio);

        BufferedImage resized = new BufferedImage(newWidth, newHeight, original.getType() == 0 ? BufferedImage.TYPE_INT_ARGB : original.getType());
        Graphics2D g = resized.createGraphics();
        
        // Apply high-quality rendering hints
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        
        g.drawImage(original, 0, 0, newWidth, newHeight, null);
        g.dispose();

        return resized;
    }

    /**
     * Converts a Java Color object to its HTML hexadecimal string representation.
     *
     * @param color The Color object to convert.
     * @return The HTML hexadecimal string (e.g., "#RRGGBB").
     */
    public static String toHtmlColor(Color color) {
        return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
    }


    /**
     * Recursively searches for a component of a specific type within a container.
     * 
     * @param <T> The type of component to find.
     * @param container The container to search.
     * @param type The class of the component type.
     * @return The first matching component found, or null if none exist.
     */
    public static <T extends Component> T findComponent(Container container, Class<T> type) {
        for (Component c : container.getComponents()) {
            if (type.isInstance(c)) {
                return type.cast(c);
            }
            if (c instanceof Container) {
                T result = findComponent((Container) c, type);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }

    /**
     * Recursively finds the innermost "leaf" component that is not a container,
     * or a container that specifically doesn't have components.
     * <p>
     * For high-fidelity frames, this method traverses viewports to find the 
     * actual text area (RSyntaxTextArea or JEditorPane).
     * </p>
     * 
     * @param component The root component to start search from.
     * @return The innermost leaf component.
     */
    public static Component findComponentLeaf(Component component) {
        if (component instanceof Container container && container.getComponentCount() > 0) {
            // Special case: for JScrollPane, we want to look inside the viewport
            if (container instanceof JScrollPane sp && sp.getViewport().getView() != null) {
                return findComponentLeaf(sp.getViewport().getView());
            }
            return findComponentLeaf(container.getComponent(0));
        }
        return component;
    }

    /**
     * Copies the given text to the system clipboard.
     *
     * @param text The text to copy.
     */
    public static void copyToClipboard(String text) {
        if (text == null) {
            return;
        }
        StringSelection selection = new StringSelection(text);
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        clipboard.setContents(selection, selection);
    }

    /**
     * Copies a Java Image to the system clipboard.
     * <p>
     * Implementation details:
     * Supports both {@link DataFlavor#imageFlavor} and {@link DataFlavor#javaFileListFlavor}.
     * The latter is achieved by saving a temporary PNG of the image, allowing it to 
     * be pasted directly into the OS filesystem (e.g., File Explorer).
     * </p>
     * 
     * @param image The image to copy. Must not be null.
     */
    public static void copyImageToClipboard(Image image) {
        Objects.requireNonNull(image, "Image to copy cannot be null.");
        
        Transferable transferable = new Transferable() {
            @Override
            public DataFlavor[] getTransferDataFlavors() {
                return new DataFlavor[]{DataFlavor.imageFlavor, DataFlavor.javaFileListFlavor};
            }

            @Override
            public boolean isDataFlavorSupported(DataFlavor flavor) {
                return DataFlavor.imageFlavor.equals(flavor) || DataFlavor.javaFileListFlavor.equals(flavor);
            }

            @Override
            public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
                if (DataFlavor.imageFlavor.equals(flavor)) {
                    return image;
                }
                
                if (DataFlavor.javaFileListFlavor.equals(flavor)) {
                    // 1. Convert to BufferedImage if necessary
                    BufferedImage bi;
                    if (image instanceof BufferedImage buff) {
                        bi = buff;
                    } else {
                        bi = new BufferedImage(image.getWidth(null), image.getHeight(null), BufferedImage.TYPE_INT_ARGB);
                        Graphics2D g = bi.createGraphics();
                        g.drawImage(image, 0, 0, null);
                        g.dispose();
                    }
                    
                    // 2. Save to a temporary file
                    File tempFile = File.createTempFile("anahata-diagram-", ".png");
                    tempFile.deleteOnExit();
                    ImageIO.write(bi, "PNG", tempFile);
                    
                    // 3. Return as a list of files
                    return List.of(tempFile);
                }
                
                throw new UnsupportedFlavorException(flavor);
            }
        };
        
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(transferable, null);
    }

    /**
     * Redispatches a {@link MouseWheelEvent} to the appropriate ancestor scroll pane.
     * <p>
     * <b>Boundary Awareness:</b> If the immediately enclosing scroll pane has vertical 
     * scrolling enabled, the event is only forwarded if the scroll pane is at its 
     * boundary (Top while rolling up, or Bottom while rolling down). 
     * </p>
     * 
     * @param component The component receiving the event.
     * @param e The mouse wheel event.
     */
    public static void redispatchMouseWheelEvent(Component component, MouseWheelEvent e) {
        if (e.getScrollType() != MouseWheelEvent.WHEEL_UNIT_SCROLL || e.getWheelRotation() == 0) {
            return;
        }

        Container parent = component.getParent();
        while (parent != null) {
            if (parent instanceof JScrollPane sp) {
                boolean verticalEnabled = sp.getVerticalScrollBarPolicy() != JScrollPane.VERTICAL_SCROLLBAR_NEVER && 
                                        sp.getVerticalScrollBar().isEnabled();
                
                if (verticalEnabled) {
                    JScrollBar vBar = sp.getVerticalScrollBar();
                    int rotation = e.getWheelRotation();
                    // We use small epsilon/inclusive checks for boundary detection
                    boolean atTop = vBar.getValue() <= vBar.getMinimum();
                    boolean atBottom = (vBar.getValue() + vBar.getVisibleAmount()) >= vBar.getMaximum();
                    
                    // If not at boundary, this scroll pane consumes the event
                    if ((rotation < 0 && !atTop) || (rotation > 0 && !atBottom)) {
                        sp.dispatchEvent(SwingUtilities.convertMouseEvent(component, e, sp));
                        return;
                    }
                    // If at boundary, we continue searching for the NEXT ancestor to forward to
                } else {
                    // Vertical is disabled, always forward to next ancestor
                }
            }
            parent = parent.getParent();
        }
        
        // Final fallback: dispatch to the root scroll pane if we reached the top of the hierarchy
        JScrollPane rootScroll = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, component);
        if (rootScroll != null) {
            rootScroll.dispatchEvent(SwingUtilities.convertMouseEvent(component, e, rootScroll));
        }
    }

    /**
     * Displays a modal dialog with a syntax-highlighted code block.
     * 
     * @param parent The parent component.
     * @param title The dialog title.
     * @param text The text to display.
     * @param language The language for syntax highlighting.
     */
    public static void showCodeBlockDialog(Component parent, String title, String text, String language) {
        AgiPanel agiPanel = (AgiPanel) SwingUtilities.getAncestorOfClass(AgiPanel.class, parent);
        if (agiPanel == null) {
            log.warn("Could not find AgiPanel ancestor for showCodeBlockDialog.");
            return;
        }

        Window ancestorWindow = SwingUtilities.getWindowAncestor(parent);
        JDialog dialog;
        if (ancestorWindow instanceof JDialog) {
            dialog = new JDialog((JDialog) ancestorWindow, title, Dialog.ModalityType.MODELESS);
        } else if (ancestorWindow instanceof JFrame) {
            dialog = new JDialog((JFrame) ancestorWindow, title, Dialog.ModalityType.MODELESS);
        } else {
            dialog = new JDialog((JFrame) null, title, Dialog.ModalityType.MODELESS);
        }
        dialog.setLayout(new BorderLayout());
        dialog.setPreferredSize(new Dimension(1000, 800));

        // THE SINGULARITY PATH
        if ("mermaid".equalsIgnoreCase(language)) {
            MermaidCodeBlockSegmentRenderer renderer = new MermaidCodeBlockSegmentRenderer(agiPanel, text, language);
            renderer.setVerticalScrollEnabled(true);
            renderer.render();
            dialog.add(renderer.getComponent(), BorderLayout.CENTER);
        } else {
            // Direct ResourceUI Rendering (High-fidelity and clutter-free for dialogs)
            StringHandle handle = new StringHandle("popup." + language, text);
            Resource resource = new Resource(handle);
            try {
                resource.reloadIfNeeded();
            } catch (Exception ex) {
                log.error("Failed to sense popup resource: {}", language, ex);
            }

            ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
            if (strategy != null) {
                JComponent viewer = strategy.createContent(resource, agiPanel);
                if (viewer instanceof AbstractTextResourceViewer atv) {
                    atv.setToolbarVisible(true); // Integrated actions (Copy)
                    atv.setReadOnly(true); // Strictly read-only for popups
                    atv.setVerticalScrollEnabled(true); // Bidirectional navigation
                    atv.setPreviewAsEditor(true); // High-fidelity snippet mode
                }
                dialog.add(viewer, BorderLayout.CENTER);
            } else {
                dialog.add(new JLabel("No ResourceUI strategy registered."), BorderLayout.CENTER);
            }
        }

        dialog.pack();
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }

    /**
     * Executes the given runnable on the Event Dispatch Thread (EDT).
     * If the current thread is already the EDT, it is executed immediately.
     * 
     * @param runnable The code to execute.
     */
    public static void runInEDT(Runnable runnable) {
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            SwingUtilities.invokeLater(runnable);
        }
    }

    /**
     * Executes the given runnable on the Event Dispatch Thread (EDT) and waits for it to complete.
     * If the current thread is already the EDT, it is executed immediately.
     * 
     * @param runnable The code to execute.
     * @throws InterruptedException if the thread is interrupted while waiting.
     * @throws InvocationTargetException if an exception occurs during execution.
     */
    public static void runInEDTAndWait(Runnable runnable) throws InterruptedException, InvocationTargetException {
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            SwingUtilities.invokeAndWait(runnable);
        }
    }
}
