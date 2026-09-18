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
import java.nio.file.Path;
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
        /**
         * The unique UUID identifier of the shared region.
         */
        private String id;
        /**
         * The rectangular coordinates and dimensions of the screen area.
         */
        private Rectangle bounds;
        /**
         * The user-assigned or generated descriptive name for the region.
         */
        private String name;
    }

    /** The list of physical device indexes currently being shared in multimodal turns. */
    @Getter
    private final List<Integer> sharedDeviceIndexes = new ArrayList<>();
    
    /** The list of rectangular regions currently being shared in multimodal turns. */
    @Getter
    private final List<SharedRegion> sharedRegions = new ArrayList<>();

    /**
     * Gets the total number of items currently being shared (displays plus custom regions).
     *
     * @return The total count of shared displays and regions.
     */
    public int getSharedCount() {
        return sharedDeviceIndexes.size() + sharedRegions.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
                "**Screens Toolkit Instructions**:\n"
                + "- You can use these tools to 'see' the user's screen or specific windows.\n"
                + "- **Live Screen Sharing**: If you see BlobParts in the RAG message, these are live captures of shared screens or regions. Each BlobPart is preceded by a text part identifying the source. Live screen captures are streamed in-memory.\n"
                + "- **Displaying Disk Images**: When referencing image files that have been written to disk by tools (such as takeScreenshot or screenshotAllWindows) or any other images on disk or from a URL, use Markdown attributes syntax in your text response: `![Screenshot](file:///path/to/image.png){width=500}`. This ensures it fits nicely in the chat without stretching the UI.\n"
                + "- **Markdown & HTML Rendering Support in Swing**: The chat renders markdown via Flexmark converted to HTML inside Swing's JEditorPane (supporting tables, lists, basic styling, autolinks, and images). **It does NOT support JavaScript-dependent extensions such as LaTeX math syntax (e.g. `$...$` or `$$...$$` formulas/arrows) or dynamic client-side scripts, which will not render properly. So DON'T OUTPUT THOSE SYMBOLS**"
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
    @AgiTool("Starts or stops live sharing a physical screen in multimodal turns (streamed in-memory, not written to disk).")
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
    @AgiTool("Adds a specific rectangular region to live screen sharing in multimodal turns (streamed in-memory, not written to disk).")
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
    @AgiTool("Takes a screenshot of a specific graphics device, attaches it to the tool response, writes it to the local file system and returns the absolute path of the file")
    public String takeScreenshot(
            @AgiToolParam("The index of the device to capture (0 for primary).") int deviceIdx) throws Exception {
        log("capturing screen " + deviceIdx);
        Path file = UICapture.screenshotToFile(deviceIdx);
        file.toFile().deleteOnExit();
        log("screenshot saved to " + file.toAbsolutePath().toString() + " and marked it for deleteOnExit() attaching to tool response...");
        addAttachment(file);
        log("screenshot attached to tool response");
        return file.toAbsolutePath().toString();
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
    @AgiTool("Takes screenshots of all visible swing application windows, writes them to disk and attaches them to the tool output. Returns the absolute path of all screenshots")
    public List<String> screenshotAllWindows() throws Exception {
        log("Taking screenshot of all visible application windows");
        List<Path> files = UICapture.screenshotAllWindows();
        List<String> ret = new ArrayList<>();
        for (Path file : files) {
            log("attaching " + file + " to tool response and marking it for deleteOnExit()");
            addAttachment(file);
            file.toFile().deleteOnExit();
            log("attached " + file + " to tool response and marked it for deleteOnExit()");
            ret.add(file.toAbsolutePath().toString());
        }
        return ret;
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
    @AgiTool("Captures a screenshot of a specific region of the primary screen, writes it to disk, attaches it to the tool response, and returns the absolute file path.")
    public String captureRegion(
            @AgiToolParam("X coordinate of the top-left corner.") int x,
            @AgiToolParam("Y coordinate of the top-left corner.") int y,
            @AgiToolParam("Width of the region.") int width,
            @AgiToolParam("Height of the region.") int height) throws Exception {
        Rectangle screenRect = new Rectangle(x, y, width, height);
        log("Capturing screenshot for : " + screenRect);
        BufferedImage capture = new Robot().createScreenCapture(screenRect);
        log("Captured BufferedImage");

        Path screenshotDir = AbstractAsiContainer.getWorkDirSubDir("screenshots");
        Path file = screenshotDir.resolve("region_" + System.currentTimeMillis() + ".png");
        log("Writing to " + file.toFile());
        ImageIO.write(capture, "png", file.toFile());
        log("Marking file as deleteOnExit() ");
        file.toFile().deleteOnExit();

        log("Attaching to tool response");
        addAttachment(file);
        log("Region captured, written to disk and attached to tool response.");
        return file.toAbsolutePath().toString();
    }
}
