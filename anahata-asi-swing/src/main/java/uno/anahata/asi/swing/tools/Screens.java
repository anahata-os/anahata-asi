/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.tools;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.internal.UICapture;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for capturing screenshots of the host system's displays and 
 * application windows.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for capturing screenshots.")
public class Screens extends AnahataToolkit {

    /**
     * Takes a screenshot of a specific graphics device.
     * 
     * @param deviceIdx The index of the device.
     * @return A status message.
     * @throws IOException if the capture fails.
     */
    @AiTool("Takes a screenshot of a specific graphics device.")
    public String takeScreenshot(
            @AiToolParam("The index of the device to capture (0 for primary).") int deviceIdx) throws IOException {
        java.nio.file.Path file = UICapture.screenshotScreen(deviceIdx);
        addAttachment(file);
        return "Screenshot of device " + deviceIdx + " captured and attached.";
    }

    /**
     * Takes screenshots of all visible application windows.
     * 
     * @return A status message.
     * @throws Exception if the capture fails.
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
