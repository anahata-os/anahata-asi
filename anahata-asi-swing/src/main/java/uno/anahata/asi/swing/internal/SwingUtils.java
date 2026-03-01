/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.internal;

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
import java.awt.datatransfer.StringSelection;
import java.awt.event.MouseWheelEvent;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;

/**
 * A collection of general-purpose Swing utility methods, primarily for image
 * manipulation and UI component creation.
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
     * Displays a modal error dialog with the given task name, description, and throwable.
     * This method uses a custom ExceptionDialog to ensure proper formatting of stack traces.
     *
     * @param component the component the dialog will be relative to
     * @param taskName The name of the task that failed.
     * @param description A brief description of the error.
     * @param throwable The Throwable object representing the exception.
     */
    public static void showException(java.awt.Component component, String taskName, String description, Throwable throwable) {
        ExceptionDialog.show(component, taskName, description, throwable);
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
     * Redispatches a {@link MouseWheelEvent} to the first ancestor {@link JScrollPane} that
     * actually supports vertical scrolling. This is used to "pass through" scroll events
     * from nested components (like code blocks) to the main conversation scroll pane.
     * 
     * @param component The component receiving the event.
     * @param e The mouse wheel event.
     */
    public static void redispatchMouseWheelEvent(Component component, MouseWheelEvent e) {
        Container parent = component.getParent();
        while (parent != null) {
            if (parent instanceof JScrollPane sp) {
                // We search for an ancestor scroll pane that allows vertical scrolling.
                // This skips the local scroll pane of the code block (which has policy NEVER).
                if (sp.getVerticalScrollBarPolicy() != JScrollPane.VERTICAL_SCROLLBAR_NEVER && 
                    sp.getVerticalScrollBar().isEnabled()) {
                    sp.dispatchEvent(SwingUtilities.convertMouseEvent(component, e, sp));
                    return;
                }
            }
            parent = parent.getParent();
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

        AbstractCodeBlockSegmentRenderer renderer = agiPanel.getAgiConfig().getEditorKitProvider().createRenderer(agiPanel, text, language);
        renderer.render();

        // For the popup, we want both scrollbars, so we wrap the INNER component
        JScrollPane popupScrollPane = new JScrollPane(renderer.getInnerComponent());
        popupScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        popupScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        dialog.add(popupScrollPane, BorderLayout.CENTER);
        dialog.pack();
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }

    /**
     * Displays a modal dialog with a syntax-highlighted JSON block.
     * 
     * @param parent The parent component.
     * @param title The dialog title.
     * @param json The JSON string to display.
     */
    public static void showJsonDialog(Component parent, String title, String json) {
        String prettyJson = JacksonUtils.prettyPrintJsonString(json);
        showCodeBlockDialog(parent, title, prettyJson, "json");
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
