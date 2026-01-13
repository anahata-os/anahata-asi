/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.internal;

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
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.chat.render.CodeBlockSegmentRenderer;

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
     * Redispatches a {@link MouseWheelEvent} to the parent {@link JScrollPane} of a component.
     * This is useful for nested scrollable components where the inner component should not
     * consume vertical scroll events.
     * 
     * @param component The component receiving the event.
     * @param e The mouse wheel event.
     */
    public static void redispatchMouseWheelEvent(Component component, MouseWheelEvent e) {
        JScrollPane parentScrollPane = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, component);
        if (parentScrollPane != null) {
            parentScrollPane.dispatchEvent(SwingUtilities.convertMouseEvent(component, e, parentScrollPane));
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
        ChatPanel chatPanel = (ChatPanel) SwingUtilities.getAncestorOfClass(ChatPanel.class, parent);
        if (chatPanel == null) {
            log.warn("Could not find ChatPanel ancestor for showCodeBlockDialog.");
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

        CodeBlockSegmentRenderer renderer = new CodeBlockSegmentRenderer(chatPanel, text, language);
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
}
