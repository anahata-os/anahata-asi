/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.awt.AWTException;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;

/**
 * A native-aware utility for capturing visual snapshots of the host's desktop 
 * environment and individual Swing frames.
 * <p>
 * This class serves as the <b>Vision Bridge</b> for the ASI, allowing it to 
 * observe both its own UI and other application windows currently visible 
 * to the user. It manages the storage and lifecycle of screenshot artifacts 
 * within the ASI work directory.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class UICapture {

    /** Standard timestamp format for screenshot filenames to ensure chronological ordering. */
    public static final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyyMMdd-HHmmss");
    
    /** The dedicated subdirectory within the ASI working folder for visual artifacts. */
    public static final Path SCREENSHOTS_DIR = AbstractAsiContainer.getWorkDirSubDir("screenshots");
    
    /**
     * Captures an image of every physical display device connected to the host system.
     * <p>
     * This method uses {@link java.awt.Robot} to perform low-level hardware scrapes, 
     * ensuring that the screenshots reflect exactly what the user sees on their monitors.
     * </p>
     * 
     * @return A list of Paths to the generated PNG artifacts.
     * @throws IOException if the native capture or file write operation fails.
     */
    public static List<Path> screenshotAllScreens() throws Exception {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] screens = ge.getScreenDevices();
        List<Path> paths = new ArrayList<>();
        for (int i = 0; i < screens.length; i++) {
            paths.add(screenshotToFile(i));
        }
        return paths;
    }


    /**
     * Performs a hardware screen capture using environment-aware safe bounds.
     * 
     * @param bounds The bounds
     * @return The captured image.
     * @throws AWTException if the Robot cannot be initialized.
     */
    public static BufferedImage getSafeScreenCapture(Rectangle bounds) throws AWTException {
        
        if (SwingUtilities.isEventDispatchThread()) {
            throw new IllegalStateException("Robot().createScreenCapture cannot be called in the EDT");
        }
        return new Robot().createScreenCapture(bounds);
    }
    
    /**
     * Takes a scerenshot of a given screen, must be called on the EDT.
     * 
     * @param gd the screen
     * @return the image
     * @throws AWTException 
     */
    public static BufferedImage getSafeScreenCapture(GraphicsDevice gd) throws AWTException {
        return getSafeScreenCapture(gd.getDefaultConfiguration().getBounds());
    }

    /**
     * Captures a high-fidelity image of a specific physical display device.
     * <p>
     * This method is the atomic operation for multi-screen capture. It ensures 
     * that the requested display index exists before attempting the hardware scrape.
     * It uses {@link #getSafeScreenCapture(GraphicsDevice)} to avoid Wayland-specific issues.
     * </p>
     * 
     * @param deviceIdx The 0-based index of the graphics device.
     * @return The Path to the generated PNG artifact.
     * @throws IOException if the capture or disk write operation fails.
     */
    public static Path screenshotToFile(int deviceIdx) throws Exception {
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] screens = ge.getScreenDevices();
        if (deviceIdx < 0 || deviceIdx >= screens.length) {
            throw new IOException("Invalid screen device index: " + deviceIdx);
        }

        Files.createDirectories(SCREENSHOTS_DIR);

        BufferedImage screenshot;
        
        screenshot = getSafeScreenCapture(screens[deviceIdx].getDefaultConfiguration().getBounds());
        

        String timestamp = TIMESTAMP_FORMAT.format(new Date());
        String filename = "screenshot-" + deviceIdx + "-" + timestamp + ".png";
        Path file = SCREENSHOTS_DIR.resolve(filename);

        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            ImageIO.write(screenshot, "png", baos);
            Files.write(file, baos.toByteArray());
        }

        file.toFile().deleteOnExit();
        return file;
    }

    /**
     * Enumerates and captures all visible application windows currently 
     * registered with the AWT Frame manager.
     * <p>
     * <b>EDT Awareness:</b> This method orchestrates a synchronous wait on the EDT 
     * to perform the {@code paint()} operation safely, ensuring visual 
     * consistency with the current UI state.
     * </p>
     * 
     * @return A list of Paths to the captured window snapshots.
     * @throws InterruptedException if the EDT wait is interrupted.
     * @throws InvocationTargetException if the paint operation fails.
     * @throws IOException if the file write fails.
     */
    public static List<Path> screenshotAllWindows() throws InterruptedException, InvocationTargetException, IOException {
        log.debug("Starting screenshot capture of all windows.");
        List<Path> ret = new ArrayList<>();
        
        Files.createDirectories(SCREENSHOTS_DIR);
        
        // CRITICAL FIX: The painting operation must run on the EDT.
        SwingUtils.runInEDTAndWait(() -> {
            try {
                java.awt.Frame[] frames = java.awt.Frame.getFrames();
                log.debug("Found {} total frames.", frames.length);
                int capturedCount = 0;
                for (java.awt.Frame frame : frames) {
                    log.debug("Checking frame: title='{}', class='{}', isShowing={}", frame.getTitle(), frame.getClass().getName(), frame.isShowing());
                    if (frame instanceof JFrame && frame.isShowing()) {
                        JFrame jframe = (JFrame) frame;
                        
                        // 1. Create image buffer
                        BufferedImage image = new BufferedImage(jframe.getWidth(), jframe.getHeight(), BufferedImage.TYPE_INT_ARGB);
                        
                        // 2. Paint on EDT
                        jframe.paint(image.getGraphics());

                        // 3. Save to file (still on EDT, but fast enough for small frames)
                        String title = jframe.getTitle();
                        if (title == null || title.trim().isEmpty()) {
                            title = "Untitled";
                        }
                        // Sanitize title for use in filename
                        String sanitizedTitle = title.replaceAll("[^a-zA-Z0-9.-]", "_");
                        String timestamp = TIMESTAMP_FORMAT.format(new Date());
                        String fileName = sanitizedTitle + "-screenshot-" + timestamp + ".png"; 
                        Path tempFile = SCREENSHOTS_DIR.resolve(fileName);
                        
                        // Write image to a byte array first, then to file
                        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                            ImageIO.write(image, "png", baos);
                            Files.write(tempFile, baos.toByteArray());
                        }
                        
                        tempFile.toFile().deleteOnExit();
                        ret.add(tempFile);
                        capturedCount++;
                        log.info("Captured window screenshot '{}'", title);
                    }
                }
                if (capturedCount == 0) {
                    log.warn("No visible application windows were found to capture.");
                }
                log.info("Finished screenshot capture. Captured {} windows.", capturedCount);
            } catch (IOException ex) {
                // Re-throw as a RuntimeException to be caught by invokeAndWait
                throw new RuntimeException("Window capture failed on EDT", ex);
            }
        });

        return ret;
    }

    /**
     * Captures a single Swing component by painting its current state 
     * into an off-screen buffer.
     * <p>
     * This is the most granular vision tool, used for capturing specific 
     * UI elements (like a single chart or a conversation part) without 
     * capturing the surrounding window clutter.
     * </p>
     * 
     * @param comp The component to capture.
     * @return The raw PNG bytes of the capture.
     * @throws IOException if the capture or encoding fails.
     */
    public static byte[] screenshotComponent(java.awt.Component comp) throws IOException {
        final BufferedImage[] imageHolder = new BufferedImage[1];
        try {
            SwingUtils.runInEDTAndWait(() -> {
                imageHolder[0] = new BufferedImage(comp.getWidth(), comp.getHeight(), BufferedImage.TYPE_INT_ARGB);
                comp.paint(imageHolder[0].getGraphics());
            });
        } catch (Exception e) {
            throw new IOException("Failed to capture component screenshot on EDT", e);
        }
        
        BufferedImage image = imageHolder[0];
        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            ImageIO.write(image, "png", baos);
            return baos.toByteArray();
        }
    }
}
