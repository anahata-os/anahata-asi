/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.browser;

import lombok.ToString;
import org.openqa.selenium.WebDriver;

/**
 * Represents a single browser WebDriver connection and its associated state.
 * <p>
 * This acts as the universal tracking object for headless and visible browser
 * sessions across different implementations (e.g., Chrome, Firefox).
 * </p>
 * 
 * @author anahata
 */
@ToString
public class BrowserDrone {

    /** 
     * The active WebDriver instance. Marked transient as the live network 
     * connection cannot be persisted across session restarts.
     */
    public transient WebDriver driver;

    /** The unique identifier for this drone. */
    public String id;

    /** The remote debugging port used to connect to the browser. */
    public int port = -1;

    /** The browser profile directory name (e.g., 'Default', 'Profile 1'). */
    public String profile;

    /** The absolute path to the user data directory containing the profiles. */
    public String userDataDir;

    /** Indicates whether this drone is running in an invisible headless mode. */
    public boolean headless;

    /** Tracks the current URL of the drone's active tab. */
    public String currentUrl;

    /** Stores the last error message encountered by this drone, if any. */
    public String lastError;

    /** 
     * Indicates whether the drone is currently in the process of starting up.
     * Marked transient as it only applies to the current runtime execution.
     */
    public transient boolean initializing = false;
}
