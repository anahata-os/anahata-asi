/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.firefox;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxDriverService;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.firefox.GeckoDriverService;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.yam.tools.browser.AbstractBrowser;
import uno.anahata.asi.yam.tools.browser.BrowserDrone;

/**
 * A toolkit for web automation and form filling using Firefox and Selenium.
 * <p>
 * This toolkit manages a fleet of {@link BrowserDrone} instances utilizing the
 * GeckoDriver for Firefox automation.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for web automation and form filling using Firefox and Selenium.")
public class Firefox extends AbstractBrowser {

    /**
     * {@inheritDoc}
     * <p>
     * Initializes the toolkit, disabling it by default if it is in beta.</p>
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Rebinds the transient driver states after session deserialization.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding Firefox toolkit.");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Clears orphaned drones after session deserialization, as Firefox cannot
     * reliably reconnect.</p>
     */
    @Override
    public void postActivate() {
        log.info("Post-activating Firefox toolkit. Clearing orphaned drones.");
        drones.clear();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides context-specific instructions to the ASI regarding Firefox
     * automation.</p>
     */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
                "**Firefox Toolkit Instructions**:\n"
                + "- **Connection Protocol**: Use the `connect()` tool to launch a Firefox browser instance.\n"
                + "- **Multi-Drone Routing**: All methods require a `droneId` to target the specific browser session.\n"
                + "- **Missing Binaries**: If Selenium cannot find the browser binary, use the `Shell` toolkit to locate it (e.g., `find /snap -name firefox`) and pass the absolute path to the `binaryPath` parameter."
        );
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates the RAG message with current Firefox environment state.</p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder("## Firefox Environment\n");
        sb.append("### Connected Drones\n");
        if (drones.isEmpty()) {
            sb.append("*No drones connected.*\n");
        } else {
            sb.append("| Drone ID | Headless | Profile | User Data Dir | Status | Current URL |\n");
            sb.append("|---|---|---|---|---|---|\n");
            for (BrowserDrone d : drones.values()) {
                String state = d.initializing ? "Initializing" : (d.driver != null ? "Connected" : "Disconnected");
                if (d.lastError != null) {
                    state += " (Error)";
                }
                String url = d.currentUrl != null ? d.currentUrl : "N/A";
                sb.append(String.format("| %s | %b | %s | %s | %s | %s |\n",
                        d.id, d.headless, d.profile, d.userDataDir, state, url));
            }
        }
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Configures {@link FirefoxOptions} with 
     * headless and profile flags. Launches the process via the system's 
     * default {@link GeckoDriverService}.</p>
     * @param droneId   The unique ID for the drone.
     * @param profile   The Firefox profile name.
     * @param headless  Whether to run without a GUI.
     * @param dataDir   The user data directory.
     * @param binaryPath The path to the Firefox binary.
     * @return A status message confirming the launch.
     * @throws AgiToolException If the drone ID is already in use.
     */
    @AgiTool("Connects to or launches a Firefox browser instance.")
    public String connect(
            @AgiToolParam(value = "A unique ID for this drone.", required = true) String droneId,
            @AgiToolParam(value = "An optional profile name to force.", required = false) String profile,
            @AgiToolParam(value = "Whether to launch headless. Default false.", required = false) Boolean headless,
            @AgiToolParam(value = "Custom user data dir.", required = false) String dataDir,
            @AgiToolParam(value = "Optional path to the Firefox binary (e.g. for Snap installs).", required = false) String binaryPath) throws AgiToolException {

        if (drones.containsKey(droneId)) {
            throw new AgiToolException("Drone ID '" + droneId + "' already exists. Please choose a different ID or close it first.");
        }

        BrowserDrone drone = new BrowserDrone();
        drone.id = droneId;
        drone.profile = profile != null ? profile : "default";
        drone.userDataDir = dataDir != null ? dataDir : System.getProperty("user.home") + "/.mozilla/firefox";
        drone.headless = (headless != null && headless);
        drone.binaryPath = binaryPath;

        String res = launchFirefoxInternal(drone);
        if (drone.driver != null) {
            drones.put(droneId, drone);
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String killAll() {
        drones.clear();
        long myPid = ProcessHandle.current().pid();
        try {
            List<ProcessHandle> toKill = ProcessHandle.allProcesses()
                    .filter(p -> p.pid() != myPid && p.info().command().orElse("").toLowerCase().matches(".*firefox.*|.*geckodriver.*"))
                    .toList();
            int count = toKill.size();
            toKill.forEach(ProcessHandle::destroy);
            for (int i = 0; i < 20; i++) {
                if (toKill.stream().noneMatch(ProcessHandle::isAlive)) {
                    break;
                }
                Thread.sleep(100);
            }
            toKill.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            return "Terminated " + count + " Firefox-related processes.";
        } catch (Exception e) {
            log.error("Failed during Firefox killAll cleanup", e);
            return "Error during cleanup: " + e.getMessage();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> listTabs(String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return Collections.singletonList("No active session for drone: " + droneId);
        }

        List<String> handles = new ArrayList<>(driver.getWindowHandles());
        String current = null;
        try {
            current = driver.getWindowHandle();
        } catch (Exception e) {
            log.error("Could not determine current window handle for drone {}", droneId, e);
        }

        List<String> tabs = new ArrayList<>();
        boolean switched = false;

        for (int i = 0; i < handles.size(); i++) {
            String handle = handles.get(i);
            String title = "Unknown";
            String url = "Unknown";

            try {
                driver.switchTo().window(handle);
                switched = true;
                title = driver.getTitle();
                url = driver.getCurrentUrl();
            } catch (Exception e) {
                log.error("Failed to switch to window handle {}", handle, e);
                title = "[Error: " + e.getMessage() + "]";
            }

            String marker = (current != null && handle.equals(current)) ? " [CURRENT]" : "";
            tabs.add(i + ": " + title + " (" + url + ")" + marker);
        }

        if (switched && current != null) {
            try {
                driver.switchTo().window(current);
            } catch (Exception e) {
                log.error("Failed to restore original window handle {} for drone {}", current, droneId, e);
            }
        }
        return tabs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized WebDriver getDriver(String droneId) {
        BrowserDrone d = drones.get(droneId);
        if (d == null) {
            return null;
        }
        if (d.driver != null) {
            try {
                d.currentUrl = d.driver.getCurrentUrl();
                return d.driver;
            } catch (Exception e) {
                log.warn("Drone '{}' unresponsive, attempting reconnection...", droneId, e);
                d.driver = null;
            }
        }
        // Note: Reconnecting to an existing Firefox process is significantly harder 
        // than Chrome because Marionette port discovery is obscure. For now, we 
        // rely on the driver staying alive.
        return d.driver;
    }

    /**
     * Executes the native Firefox launch sequence. 
     * <p>Implementation details: Uses {@link CompletableFuture} to launch 
     * the driver in a background thread to prevent blocking the tool 
     * execution flow. Sets a 60-second timeout for the initial handshake.</p>
     * @param d The drone to launch.
     * @return A success or error message.
     */
    private synchronized String launchFirefoxInternal(BrowserDrone d) {
        d.initializing = true;
        d.lastError = null;
        if (d.driver != null) {
            try {
                d.driver.quit();
            } catch (Exception e) {
                log.error("Error quitting previous driver for drone {}", d.id, e);
            }
        }

        FirefoxOptions options = new FirefoxOptions();
        if (d.headless) {
            options.addArguments("-headless");
        }
        if (d.profile != null && !d.profile.isEmpty() && !d.profile.equals("default")) {
            options.addArguments("-P", d.profile);
        }
        if (d.binaryPath != null && !d.binaryPath.isEmpty()) {
            options.setBinary(d.binaryPath);
        }

        try {
            CompletableFuture<WebDriver> future = CompletableFuture.supplyAsync(() -> {
                return new FirefoxDriver(options);
            }, getExecutorService());

            d.driver = future.get(60, TimeUnit.SECONDS);
            log("Drone '" + d.id + "' successfully initialized. URL: " + d.driver.getCurrentUrl());
            return "Firefox drone '" + d.id + "' launched successfully.";
        } catch (Exception e) {
            d.lastError = ExceptionUtils.getStackTrace(e);
            log.error("Failed to initialize Firefox drone '{}'", d.id, e);
            return "Failed to launch Firefox drone: " + e.getMessage();
        } finally {
            d.initializing = false;
        }
    }
}
