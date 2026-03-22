/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * A hardware-aware toolkit for capturing high-fidelity screenshots of the host system's 
 * displays and individual application windows.
 * <p>
 * This toolkit leverages the {@link uno.anahata.asi.swing.internal.UICapture} utility 
 * to perform native screen scraping. It is primarily used by the ASI to "see" 
 * the user's current workspace or specific application states.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for capturing screenshots.")
public class Screens extends AnahataToolkit {

    /**
     * Captures a screenshot of a specific physical display device identified by its index.
     * <p>
     * The resulting image is automatically added as an attachment to the current 
     * tool response, making it immediately available to the model's vision system.
     * </p>
     * 
     * @param deviceIdx The 0-based index of the graphics device (0 is usually the primary display).
     * @return A descriptive status message confirming the capture and attachment.
     * @throws IOException if the native capture operation fails.
     */
    @AiTool("Takes a screenshot of a specific graphics device.")
    public String takeScreenshot(
            @AiToolParam("The index of the device to capture (0 for primary).") int deviceIdx) throws IOException {
        java.nio.file.Path file = UICapture.screenshotScreen(deviceIdx);
        addAttachment(file);
        return "Screenshot of device " + deviceIdx + " captured and attached.";
    }

    /**
     * Orchestrates a bulk capture of all visible application windows currently 
     * managed by the host's window manager.
     * <p>
     * Each window is captured as a separate image and attached to the tool response. 
     * This is particularly useful for multi-window discovery tasks.
     * </p>
     * 
     * @return A status message indicating the total number of windows captured.
     * @throws Exception if the window enumeration or capture fails.
     */
    @AiTool("Takes screenshots of all visible application windows.")
    public String screenshotAllWindows() throws Exception {
        List<java.nio.file.Path> files = UICapture.screenshotAllWindows();
        for (java.nio.file.Path file : files) {
            addAttachment(file);
        }
        return files.size() + " window(s) captured and attached.";
    }
}
