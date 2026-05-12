/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.image.BufferedImage;
import java.awt.GraphicsEnvironment;
import java.awt.GraphicsDevice;
import java.awt.MouseInfo;
import java.awt.Point;
import java.util.UUID;
import javax.imageio.ImageIO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.swing.internal.SwingUtils;
import java.util.Comparator;
import java.util.stream.IntStream;

/**
 * A hardware-aware toolkit for capturing high-fidelity screenshots of the host
 * system's displays and individual application windows.
 * <p>
 * This toolkit leverages the {@link uno.anahata.asi.swing.internal.UICapture}
 * utility to perform native screen scraping. It is primarily used by the ASI to
 * "see" the user's current workspace or specific application states.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for capturing screenshots and live screen sharing.")
public class Screens extends AnahataToolkit {

    /**
     * Represents a shared rectangular region of the screen.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SharedRegion {
        private String id;
        private Rectangle bounds;
        private String name;
    }

    /** The list of physical device indexes currently being shared in multimodal turns. */
    @Getter
    private final List<Integer> sharedDeviceIndexes = new ArrayList<>();
    
    /** The list of rectangular regions currently being shared in multimodal turns. */
    @Getter
    private final List<SharedRegion> sharedRegions = new ArrayList<>();

    /**
     * {@inheritDoc} 
     */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
                "**Screens Toolkit Instructions**:\n"
                + "- You can use these tools to 'see' the user's screen or specific windows.\n"
                + "- **Multimodal Sharing**: If you see BlobParts in the RAG message, these are live captures of shared screens or regions. "
                + "Each BlobPart is preceded by a text part identifying the source.\n"
                + "- **Displaying Images**: To show a local image file beautifully in the chat, use Markdown attributes syntax in your text response: `![Screenshot](file:///path/to/image.png){width=500}`. This ensures it fits perfectly and prevents stretching the UI."
        );
    }

    /**
     * {@inheritDoc} 
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        try {
            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] devices = ge.getScreenDevices();
            Point mouseLoc = MouseInfo.getPointerInfo().getLocation();

            StringBuilder status = new StringBuilder("## Display & Pointer Status\n");
            status.append("- **Mouse Position**: X=").append(mouseLoc.x).append(", Y=").append(mouseLoc.y).append("\n");

            // Sort indices based on physical X coordinate for a natural logical layout
            List<Integer> sortedIndices = IntStream.range(0, devices.length)
                    .boxed()
                    .sorted(Comparator.comparingInt(i -> devices[i].getDefaultConfiguration().getBounds().x))
                    .toList();

            for (int i : sortedIndices) {
                GraphicsDevice gd = devices[i];
                Rectangle bounds = gd.getDefaultConfiguration().getBounds();
                boolean hasMouse = bounds.contains(mouseLoc);
                status.append(String.format("- **Screen %d**: %s | Bounds: x=%d, y=%d, w=%d, h=%d %s\n",
                        i, gd.getIDstring(), bounds.x, bounds.y, bounds.width, bounds.height, hasMouse ? "[MOUSE HERE]" : ""));
            }
            ragMessage.addTextPart(status.toString());
            
            Robot robot = new Robot();
            // 1. Physical Screens
            for (Integer idx : sharedDeviceIndexes) {
                if (idx >= 0 && idx < devices.length) {
                    BufferedImage img = UICapture.getSafeScreenCapture(devices[idx]);
                    byte[] data = SwingUtils.encodeToPng(img);
                    
                    ragMessage.addTextPart("### Live Capture: Physical Screen " + idx);
                    ragMessage.addBlobPart("image/png", data);
                }
            }
            
            // 2. Custom Regions
            for (SharedRegion region : sharedRegions) {
                BufferedImage img = robot.createScreenCapture(region.getBounds());
                byte[] data = SwingUtils.encodeToPng(img);
                
                ragMessage.addTextPart("### Live Capture: Region '" + region.getName() + "' (" + region.getId() + ")");
                ragMessage.addBlobPart("image/png", data);
            }

        } catch (Exception e) {
            log.error("Error populating screen info", e);
            ragMessage.addTextPart("## Screen Capture Error\n- " + e.getMessage());
        }
    }



    /**
     * Toggles sharing for a specific screen device.
     * 
     * @param deviceIdx The index of the device.
     * @return A status message.
     */
    @AgiTool("Starts or stops sharing a physical screen.")
    public String toggleDeviceSharing(@AgiToolParam("The index of the device") int deviceIdx) {
        if (sharedDeviceIndexes.contains(deviceIdx)) {
            sharedDeviceIndexes.remove(Integer.valueOf(deviceIdx));
            getPropertyChangeSupport().firePropertyChange("sharingChanged", null, null);
            return "Stopped sharing Screen " + deviceIdx;
        } else {
            sharedDeviceIndexes.add(deviceIdx);
            getPropertyChangeSupport().firePropertyChange("sharingChanged", null, null);
            return "Started sharing Screen " + deviceIdx;
        }
    }
    
    /**
     * Adds a specific rectangular region to the live share.
     * 
     * @param x X coordinate.
     * @param y Y coordinate.
     * @param w Width.
     * @param h Height.
     * @param name Optional name for the region.
     * @return A status message with the ID.
     */
    @AgiTool("Adds a specific rectangular region to the live share.")
    public String startSharingRegion(
            @AgiToolParam("X coordinate") int x, 
            @AgiToolParam("Y coordinate") int y, 
            @AgiToolParam("Width") int w, 
            @AgiToolParam("Height") int h,
            @AgiToolParam("A Name for the region you are capturing") String name) {
        String id = UUID.randomUUID().toString();
        sharedRegions.add(new SharedRegion(id, new Rectangle(x, y, w, h), name != null ? name : "Region " + (sharedRegions.size() + 1)));
        getPropertyChangeSupport().firePropertyChange("sharingChanged", null, null);
        return "Started sharing region " + id;
    }
    
    /**
     * Stops sharing a specific region by its ID.
     * 
     * @param regionId The UUID of the shared region.
     * @return A status message.
     */
    @AgiTool("Stops sharing a specific region by its ID.")
    public String stopSharingRegion(@AgiToolParam("The UUID of the shared region") String regionId) {
        boolean removed = sharedRegions.removeIf(r -> r.getId().equals(regionId));
        if (removed) {
            getPropertyChangeSupport().firePropertyChange("sharingChanged", null, null);
        }
        return removed ? "Stopped sharing region " + regionId : "Region not found: " + regionId;
    }

    /**
     * Captures a screenshot of a specific physical display device identified by
     * its index.
     * <p>
     * The resulting image is automatically added as an attachment to the
     * current tool response, making it immediately available to the model's
     * vision system.
     * </p>
     *
     * @param deviceIdx The 0-based index of the graphics device (0 is usually
     * the primary display).
     * @return A descriptive status message confirming the capture and
     * attachment.
     * @throws IOException if the native capture operation fails.
     */
    @AgiTool("Takes a screenshot of a specific graphics device.")
    public String takeScreenshot(
            @AgiToolParam("The index of the device to capture (0 for primary).") int deviceIdx) throws Exception {
        java.nio.file.Path file = UICapture.screenshotToFile(deviceIdx);
        addAttachment(file);
        return "Screenshot of device " + deviceIdx + " captured and attached.";
    }

    /**
     * Orchestrates a bulk capture of all visible application windows currently
     * managed by the host's window manager.
     * <p>
     * Each window is captured as a separate image and attached to the tool
     * response. This is particularly useful for multi-window discovery tasks.
     * </p>
     *
     * @return A status message indicating the total number of windows captured.
     * @throws Exception if the window enumeration or capture fails.
     */
    @AgiTool("Takes screenshots of all visible application windows.")
    public String screenshotAllGraphicsDevices() throws Exception {
        List<java.nio.file.Path> files = UICapture.screenshotAllWindows();
        for (java.nio.file.Path file : files) {
            addAttachment(file);
        }
        return files.size() + " window(s) captured and attached.";
    }

    /**
     * Captures a specific region of the primary screen.
     *
     * @param x X coordinate of the top-left corner.
     * @param y Y coordinate of the top-left corner.
     * @param width Width of the region.
     * @param height Height of the region.
     * @return A status message with the file path.
     * @throws Exception if the native capture operation fails.
     */
    @AgiTool("Captures a screenshot of a specific region of the primary screen.")
    public String captureRegion(
            @AgiToolParam("X coordinate of the top-left corner.") int x,
            @AgiToolParam("Y coordinate of the top-left corner.") int y,
            @AgiToolParam("Width of the region.") int width,
            @AgiToolParam("Height of the region.") int height) throws Exception {

        Rectangle screenRect = new Rectangle(x, y, width, height);
        BufferedImage capture = new Robot().createScreenCapture(screenRect);

        java.nio.file.Path screenshotDir = AbstractAsiContainer.getWorkDirSubDir("screenshots");
        java.nio.file.Path file = screenshotDir.resolve("region_" + System.currentTimeMillis() + ".png");
        ImageIO.write(capture, "png", file.toFile());

        addAttachment(file);
        return "Region captured and attached. Path: " + file.toAbsolutePath().toString();
    }
}
