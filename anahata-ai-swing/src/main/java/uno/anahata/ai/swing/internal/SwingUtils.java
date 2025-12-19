/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.internal;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.image.BufferedImage;
import javax.swing.SwingUtilities;
import lombok.experimental.UtilityClass;
import org.apache.commons.lang3.exception.ExceptionUtils; // Added import
import org.jdesktop.swingx.JXErrorPane;
import org.jdesktop.swingx.error.ErrorInfo;

/**
 * A collection of general-purpose Swing utility methods, primarily for image
 * manipulation and UI component creation.
 *
 * @author anahata
 */
@UtilityClass
public class SwingUtils {

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
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
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
     * Displays a modal SwingX error dialog with the given task name, description, and throwable.
     * This method assumes it is called on the Event Dispatch Thread (EDT).
     *
     * @param component the component the dialog will be relative to
     * @param taskName The name of the task that failed.
     * @param description A brief description of the error.
     * @param throwable The Throwable object representing the exception.
     */
    public static void showException(java.awt.Component component, String taskName, String description, Throwable throwable) {
        ErrorInfo errorInfo = new ErrorInfo(
                taskName,
                description,
                ExceptionUtils.getStackTrace(throwable), // Changed to get full stack trace
                "Error in " + taskName,
                throwable,
                java.util.logging.Level.SEVERE,
                null);
        Window ancestor = component != null ? SwingUtilities.getWindowAncestor(component) : null;
        JXErrorPane.showDialog(ancestor, errorInfo);
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
        Clipboard clipboard = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard();
        clipboard.setContents(selection, selection);
    }
}
