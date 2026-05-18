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

    /**
     * Indicates whether the drone is currently in the process of starting up.
     * Marked transient as it only applies to the current runtime execution.
     */
    public transient boolean initializing = false;

    /**
     * The unique identifier for this drone.
     */
    public String id;

    /**
     * The remote debugging port used to connect to the browser. 
     * <p>A value of {@code -1} indicates that no connection has been established 
     * or the port hasn't been assigned yet.</p>
     */
    public int port = -1;

    /**
     * The browser profile directory name (e.g., 'Default', 'Profile 1').
     */
    public String profile;

    /**
     * The absolute path to the user data directory containing the profiles.
     */
    public String userDataDir;

    /**
     * Indicates whether this drone is running in an invisible headless mode.
     */
    public boolean headless;

    /**
     * Optional path to the browser binary (e.g. for Snap or custom installs).
     */
    public String binaryPath;

    /**
     * Tracks the current URL of the drone's active tab. 
     * <p>This is updated after every navigation event to provide quick context 
     * without requiring a remote driver query.</p>
     */
    public String currentUrl;

    /**
     * Stores the last error message encountered by this drone, if any. 
     * <p>Used by {@link uno.anahata.asi.yam.tools.browser.AbstractBrowser#getStatus(String)} 
     * to provide diagnostic feedback when a session fails to start.</p>
     */
    public String lastError;

}
