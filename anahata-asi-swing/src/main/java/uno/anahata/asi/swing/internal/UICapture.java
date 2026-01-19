/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.internal;

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
import uno.anahata.asi.AsiContainer;

/**
 * Utility class for capturing screenshots of the host system's display devices
 * and visible application windows.
 *
 * @author anahata
 */
@Slf4j
public class UICapture {

    public static final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyyMMdd-HHmmss");
    
    // FIX: Use the correct V2 AsiContainer to get the working folder as a Path
    public static final Path SCREENSHOTS_DIR = AsiContainer.getWorkDirSubDir("screenshots");
    
    /**
     * Takes a screenshot of all connected graphics devices.
     * 
     * @return A list of Paths containing the screenshots.
     * @throws IOException if a file operation fails.
     */
    public static List<Path> screenshotAllScreenDevices() throws IOException {
        List<Path> ret = new ArrayList<>();
        
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] screens = ge.getScreenDevices();

        Files.createDirectories(SCREENSHOTS_DIR);

        for (int i = 0; i < screens.length; i++) {
            Rectangle screenBounds = screens[i].getDefaultConfiguration().getBounds();
            BufferedImage screenshot;
            try {
                screenshot = new Robot().createScreenCapture(screenBounds);
            } catch (Exception e) {
                throw new IOException("Failed to capture screen device " + i, e);
            }

            String timestamp = TIMESTAMP_FORMAT.format(new Date());
            String filename = "screen-" + i + "-" + timestamp + ".png"; 
            Path tempFile = SCREENSHOTS_DIR.resolve(filename);
            
            // Write image to a byte array first, then to file
            try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                ImageIO.write(screenshot, "png", baos);
                Files.write(tempFile, baos.toByteArray());
            }
            
            tempFile.toFile().deleteOnExit();
            ret.add(tempFile);
        }
        
        return ret;
    }

    /**
     * Takes a screenshot of all visible JFrames.
     * 
     * @return A list of Paths containing the captured frames.
     * @throws InterruptedException if the EDT operation is interrupted.
     * @throws InvocationTargetException if the EDT operation throws an exception.
     * @throws IOException if a file operation fails.
     */
    public static List<Path> screenshotAllJFrames() throws InterruptedException, InvocationTargetException, IOException {
        log.debug("Starting screenshot capture of all JFrames.");
        List<Path> ret = new ArrayList<>();
        
        Files.createDirectories(SCREENSHOTS_DIR);
        
        // CRITICAL FIX: The painting operation must run on the EDT.
        SwingUtilities.invokeAndWait(() -> {
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
                        String fileName = sanitizedTitle + "-" + timestamp + ".png"; 
                        Path tempFile = SCREENSHOTS_DIR.resolve(fileName);
                        
                        // Write image to a byte array first, then to file
                        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
                            ImageIO.write(image, "png", baos);
                            Files.write(tempFile, baos.toByteArray());
                        }
                        
                        tempFile.toFile().deleteOnExit();
                        ret.add(tempFile);
                        capturedCount++;
                        log.info("Captured frame '{}'", title);
                    }
                }
                if (capturedCount == 0) {
                    log.warn("No visible application frames were found to capture.");
                }
                log.info("Finished screenshot capture. Captured {} frames.", capturedCount);
            } catch (IOException ex) {
                // Re-throw as a RuntimeException to be caught by invokeAndWait
                throw new RuntimeException("JFrame capture failed on EDT", ex);
            }
        });

        return ret;
    }
}