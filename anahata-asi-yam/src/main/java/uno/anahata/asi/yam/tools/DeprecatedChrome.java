/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import uno.anahata.asi.yam.tools.chrome.ChromeUtils;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.Getter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.toolkit.shell.Shell;
import uno.anahata.asi.toolkit.shell.ShellExecutionResult;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

/**
 * A toolkit for web automation and form filling using Selenium.
 * <p>
 * This toolkit supports acting as the user by launching Chrome with their
 * existing profile or connecting to an already running instance.
 * </p>
 * <p>
 * <b>Note on Persistence:</b> The {@code driver} and {@code initializing}
 * fields are marked as {@code transient} because they represent active runtime
 * state that cannot be serialized. The {@code lastConnectedPort} is persisted
 * to allow auto-reconnection during {@link #rebind()}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
//@AgiToolkit("A toolkit for web automation and form filling using Chrome and Selenium (Beta).")
@Deprecated
public class DeprecatedChrome extends AnahataToolkit implements Rebindable {

    /**
     * Represents a single Chrome WebDriver connection and its associated state.
     */
    @ToString
    @Deprecated
    public static class ChromeDrone {

        /**
         * The active WebDriver instance.
         */
        public transient WebDriver driver;
        /**
         * The unique identifier for this drone.
         */
        public String id;
        /**
         * The remote debugging port.
         */
        public int port = -1;
        /**
         * The Chrome profile directory name.
         */
        public String profile;
        /**
         * The Chrome user data directory path.
         */
        public String userDataDir;
        /**
         * Whether the drone is running in headless mode.
         */
        public boolean headless;
        /**
         * The current URL of the drone's active tab.
         */
        public String currentUrl;
        /**
         * The last error encountered by this drone.
         */
        public String lastError;
        /**
         * Whether the drone is currently initializing.
         */
        public transient boolean initializing = false;
    }

    /**
     * The registry of all connected Chrome drones.
     */
    private java.util.Map<String, ChromeDrone> drones = new java.util.concurrent.ConcurrentHashMap<>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void rebind() {
        log.info("Rebinding Chrome toolkit.");
        for (ChromeDrone drone : drones.values()) {
            drone.driver = null;
            drone.initializing = false;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
                "**Chrome Toolkit Instructions**:\n"
                + "- **Beta**: This toolkit is in beta mode, encourage the user to report any issues found on github.\n"
                + "- **Connection Protocol**: Use the `connect()` tool as your primary entry point. It automatically detects running browsers and handles the restart protocol if necessary.\n"
                + "- **Extensibility & Advanced Scraping**: The `Java` toolkit has `Selenium` and `Jsoup` on its classpath. You can use it to cover any gaps in this toolkit (e.g. creating custom Selenium connectors, managing multiple concurrent headless profiles, etc). The `Shell` tool can also be used for CLI web scraping.\n"
                + "- **Scraping Tips (Headless vs Visual)**: Use the visual `Chrome` toolkit to bypass strong bot protections (DataDome, Cloudflare) by hijacking the user's authenticated fingerprint. For raw speed on unprotected sites, use `NbJava` to spawn isolated background drones.\n"
                + "- **Headless Drones via Java**: When spawning `new ChromeDriver(options)` directly on the java tool, use `--headless=new`, `--no-sandbox`, `--disable-dev-shm-usage`, and `--ignore-certificate-errors` to bypass SSL blocks and ensure stable Linux execution.\n"
                + "- **Persistent Drone Profiles**: By default, headless drones use `/tmp`. To inherit the user's session (bypassing logins/CAPTCHAs in headless mode), map their active profile using `--user-data-dir`.\n"
                + "- **Multi-Drone Routing**: All methods require a `droneId` to target the specific browser session."
        );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder("## Chrome Environment\n");
        sb.append("### Connected Drones\n");
        if (drones.isEmpty()) {
            sb.append("*No drones connected.*\n");
        } else {
            sb.append("| Drone ID | Port | Headless | Profile | User Data Dir | Status | Current URL |\n");
            sb.append("|---|---|---|---|---|---|---|\n");
            for (ChromeDrone d : drones.values()) {
                String state = d.initializing ? "Initializing" : (d.driver != null ? "Connected" : "Disconnected");
                if (d.lastError != null) {
                    state += " (Error)";
                }
                String url = d.currentUrl != null ? d.currentUrl : "N/A";
                sb.append(String.format("| %s | %d | %b | %s | %s | %s | %s |\n", d.id, d.port, d.headless, d.profile, d.userDataDir, state, url));
            }
        }
        String userDataDir = ChromeUtils.getDefaultChromeUserDataDir();
        sb.append("\n- **Detected User Data Dir**: ").append(userDataDir).append("\n");
        File dir = new File(userDataDir);
        if (dir.exists() && dir.isDirectory()) {
            File[] profiles = dir.listFiles(f -> f.isDirectory() && (f.getName().equals("Default") || f.getName().startsWith("Profile ")));
            if (profiles != null && profiles.length > 0) {
                sb.append("- **Available Profiles**: ");
                for (File p : profiles) {
                    sb.append(p.getName()).append(", ");
                }
                sb.append("\n");
            }
        }
        sb.append("\n").append(getProcessReport());
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * High-level tool to connect to the user's browser.
     *
     * @param droneId A unique ID for this drone.
     * @param profile An optional profile name to force.
     * @param headless Whether to launch headless.
     * @param dataDir Custom user data dir.
     * @return A status message.
     */
    @AgiTool("Connects to or launches a Chrome browser instance.")
    public String connect(
            @AgiToolParam(value = "A unique ID for this drone.", required = true) String droneId,
            @AgiToolParam(value = "An optional profile name to force. If null, auto-detected.", required = false) String profile,
            @AgiToolParam(value = "Whether to launch headless. Default false.", required = false) Boolean headless,
            @AgiToolParam(value = "Custom user data dir. If null, defaults to system.", required = false) String dataDir) {
        if (drones.containsKey(droneId)) {
            return "Drone ID '" + droneId + "' already exists. Please choose a different ID or close it first.";
        }
        ChromeDrone drone = new ChromeDrone();
        drone.id = droneId;
        drones.put(droneId, drone);
        drone.profile = profile;
        drone.userDataDir = dataDir != null ? dataDir : ChromeUtils.getDefaultChromeUserDataDir();
        drone.headless = (headless != null && headless);

        if (!drone.headless) {
            List<ProcessHandle> processes = ProcessHandle.allProcesses().filter(p -> p.info().command().map(c -> c.toLowerCase().contains("chrome")).orElse(false)).toList();
            int debugPort = -1;
            for (ProcessHandle p : processes) {
                String cmdLine = getCommandLine(p);
                if (cmdLine.contains("--type=") || cmdLine.contains("chrome-sandbox")) {
                    continue;
                }
                String portStr = extractArg(cmdLine, "--remote-debugging-port");
                if (portStr != null && !portStr.equals("0")) {
                    try {
                        debugPort = Integer.parseInt(portStr);
                        break;
                    } catch (NumberFormatException e) {
                    }
                }
                if (drone.profile == null) {
                    drone.profile = extractArg(cmdLine, "--profile-directory");
                }
            }
            if (debugPort != -1) {
                return connectToExistingInternal(drone, debugPort);
            }
        }
        if (drone.profile == null) {
            drone.profile = detectActiveProfile(drone.userDataDir);
        }
        if (!drone.headless) {
            killAllInternal();
            clearSingletonLockInternal(drone.userDataDir);
            clearSingletonLockInternal(new File(drone.userDataDir, drone.profile).getAbsolutePath());
            resetExitStateInternal(drone.userDataDir, drone.profile);
        }
        return launchProfileChromeInternal(drone, null);
    }

    /**
     * Gets the current status of the browser driver for a specific drone.
     *
     * @param droneId The ID of the drone.
     * @return A status report.
     */
    @AgiTool("Gets the current status of a specific drone.")
    public String getStatus(@AgiToolParam("The ID of the drone.") String droneId) {
        ChromeDrone d = drones.get(droneId);
        if (d == null) {
            return "Drone not found: " + droneId;
        }
        if (d.initializing) {
            return "Drone '" + droneId + "' is currently initializing...";
        }
        if (getDriver(droneId) == null) {
            return "No active browser session for '" + droneId + "'. Last error:\n" + (d.lastError != null ? d.lastError : "None");
        }
        try {
            return "Connected '" + droneId + "' to: " + d.currentUrl;
        } catch (Exception e) {
            return "Driver '" + droneId + "' is present but unresponsive: " + e.getMessage();
        }
    }

    /**
     * Terminates all running Chrome and ChromeDriver processes on the host
     * system.
     *
     * @return A status message.
     */
    @AgiTool("Terminates all running Chrome and ChromeDriver processes on the host system.")
    public String killAll() {
        drones.clear();
        return killAllInternal();
    }

    /**
     * Navigates the current browser session to a new URL.
     *
     * @param droneId The ID of the drone.
     * @param url The URL to navigate to.
     * @return A status message.
     */
    @AgiTool("Navigates the specified drone to a new URL.")
    public String navigate(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The URL to navigate to.") String url) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session for drone: " + droneId;
        }
        driver.get(url);
        return "Navigated drone '" + droneId + "' to: " + url;
    }

    /**
     * Takes a screenshot of the current page and attaches it to the session.
     *
     * @param droneId The ID of the drone.
     * @param name The name of the screenshot file.
     * @return A status message.
     * @throws Exception if screenshot fails.
     */
    @AgiTool("Takes a screenshot of the current page and attaches it to the session.")
    public String getScreenshot(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The name of the screenshot file.") String name) throws Exception {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session for drone: " + droneId;
        }

        File srcFile = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
        File screenshotDir = AbstractAsiContainer.getWorkDirSubDir("screenshots").toFile();
        File destFile = new File(screenshotDir, name + ".png");

        Files.copy(srcFile.toPath(), destFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        addAttachment(destFile);

        return "Screenshot '" + name + "' attached to session for drone: " + droneId;
    }

    /**
     * Lists all open tabs/windows in the current browser session.
     *
     * @param droneId The ID of the drone.
     * @return A list of tab titles, URLs, and indices.
     */
    @AgiTool("Lists all open tabs/windows in the specified drone.")
    public List<String> listTabs(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return Collections.singletonList("No active session for drone: " + droneId);
        }

        List<String> handles = new ArrayList<>(driver.getWindowHandles());
        String current = null;
        try {
            current = driver.getWindowHandle();
        } catch (Exception e) {
        }

        List<String> tabs = new ArrayList<>();
        boolean switched = false;

        for (int i = 0; i < handles.size(); i++) {
            String handle = handles.get(i);
            String title = "Unknown";
            String url = "Unknown";
            boolean cdpSuccess = false;
            if (driver instanceof ChromeDriver chromeDriver) {
                try {
                    Map<String, Object> params = Map.of("targetId", handle);
                    Map<String, Object> result = chromeDriver.executeCdpCommand("Target.getTargetInfo", params);
                    Map<String, Object> targetInfo = (Map<String, Object>) result.get("targetInfo");
                    if (targetInfo != null) {
                        title = (String) targetInfo.get("title");
                        url = (String) targetInfo.get("url");
                        cdpSuccess = true;
                    }
                } catch (Exception e) {
                }
            }
            if (!cdpSuccess) {
                try {
                    driver.switchTo().window(handle);
                    switched = true;
                    title = driver.getTitle();
                    url = driver.getCurrentUrl();
                } catch (Exception e) {
                    title = "[Error: " + e.getMessage() + "]";
                }
            }
            String marker = (current != null && handle.equals(current)) ? " [CURRENT]" : "";
            tabs.add(i + ": " + title + " (" + url + ")" + marker);
        }
        if (switched && current != null) {
            try {
                driver.switchTo().window(current);
            } catch (Exception e) {
            }
        }
        return tabs;
    }

    /**
     * Switches the active tab/window by its index.
     *
     * @param droneId The ID of the drone.
     * @param index The index of the tab.
     * @return A status message.
     */
    @AgiTool("Switches the active tab/window by its index.")
    public String switchToTab(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The index of the tab.") int index) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session for drone: " + droneId;
        }

        List<String> handles = new ArrayList<>(driver.getWindowHandles());
        if (index < 0 || index >= handles.size()) {
            return "Invalid tab index: " + index + ". Total tabs: " + handles.size();
        }

        try {
            driver.switchTo().window(handles.get(index));
            return "Switched drone '" + droneId + "' to tab: " + driver.getTitle();
        } catch (Exception e) {
            return "Failed to switch tab: " + e.getMessage();
        }
    }

    /**
     * Navigates back in the browser history.
     *
     * @param droneId The ID of the drone.
     * @return A status message.
     */
    @AgiTool("Navigates back in the browser history.")
    public String goBack(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        driver.navigate().back();
        return "Navigated back.";
    }

    /**
     * Navigates forward in the browser history.
     *
     * @param droneId The ID of the drone.
     * @return A status message.
     */
    @AgiTool("Navigates forward in the browser history.")
    public String goForward(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        driver.navigate().forward();
        return "Navigated forward.";
    }

    /**
     * Refreshes the current page.
     *
     * @param droneId The ID of the drone.
     * @return A status message.
     */
    @AgiTool("Refreshes the current page.")
    public String refresh(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        driver.navigate().refresh();
        return "Page refreshed.";
    }

    /**
     * Gets the full HTML source of the current page.
     *
     * @param droneId The ID of the drone.
     * @return The page source.
     */
    @AgiTool("Gets the full HTML source of the current page.")
    public String getPageSource(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        return driver.getPageSource();
    }

    /**
     * Gets the visible text content of the current page.
     *
     * @param droneId The ID of the drone.
     * @return The page text.
     */
    @AgiTool("Gets the visible text content of the current page.")
    public String getPageText(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        return driver.findElement(By.tagName("body")).getText();
    }

    /**
     * Inspects the current page for input fields and buttons.
     *
     * @param droneId The ID of the drone.
     * @return A summary of found elements.
     */
    @AgiTool("Inspects the current page for input fields and buttons.")
    public String inspectForm(@AgiToolParam("The ID of the drone.") String droneId) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        List<WebElement> inputs = driver.findElements(By.cssSelector("input, textarea, select"));
        StringBuilder sb = new StringBuilder("Found " + inputs.size() + " form elements:\n");
        for (WebElement input : inputs) {
            sb.append("- Tag: ").append(input.getTagName())
                    .append(", Type: ").append(input.getAttribute("type"))
                    .append(", Name: ").append(input.getAttribute("name"))
                    .append(", ID: ").append(input.getAttribute("id"))
                    .append("\n");
        }
        List<WebElement> buttons = driver.findElements(By.tagName("button"));
        sb.append("\nFound ").append(buttons.size()).append(" buttons:\n");
        for (WebElement button : buttons) {
            sb.append("- Text: ").append(button.getText())
                    .append(", ID: ").append(button.getAttribute("id"))
                    .append(", Type: ").append(button.getAttribute("type"))
                    .append("\n");
        }
        return sb.toString();
    }

    /**
     * Clicks an element on the page.
     *
     * @param droneId The ID of the drone.
     * @param identifier The ID, Name, or visible text of the element.
     * @return A status message.
     */
    @AgiTool("Clicks an element on the page.")
    public String clickElement(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The ID, Name, or visible text of the element.") String identifier) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        WebElement el = null;
        try {
            el = driver.findElement(By.id(identifier));
        } catch (Exception e) {
        }
        if (el == null) try {
            el = driver.findElement(By.name(identifier));
        } catch (Exception e) {
        }
        if (el == null) try {
            el = driver.findElement(By.linkText(identifier));
        } catch (Exception e) {
        }
        if (el == null) try {
            el = driver.findElement(By.xpath("//*[contains(text(), '" + identifier + "')]"));
        } catch (Exception e) {
        }

        if (el != null) {
            el.click();
            return "Clicked element: " + identifier;
        }
        return "Could not find element: " + identifier;
    }

    /**
     * Scrolls the specified element into view.
     *
     * @param droneId The ID of the drone.
     * @param identifier The element identifier.
     * @return A status message.
     */
    @AgiTool("Scrolls the specified element into view.")
    public String scrollToElement(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The ID, Name, or visible text of the element.") String identifier) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        WebElement el = null;
        try {
            el = driver.findElement(By.id(identifier));
        } catch (Exception e) {
        }
        if (el == null) try {
            el = driver.findElement(By.name(identifier));
        } catch (Exception e) {
        }
        if (el == null) try {
            el = driver.findElement(By.xpath("//*[contains(text(), '" + identifier + "')]"));
        } catch (Exception e) {
        }

        if (el != null) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView(true);", el);
            return "Scrolled to element: " + identifier;
        }
        return "Could not find element to scroll to: " + identifier;
    }

    /**
     * Waits for an element to be visible on the page.
     *
     * @param droneId The ID of the drone.
     * @param cssSelector The CSS selector.
     * @param timeoutSeconds The timeout.
     * @return A status message.
     */
    @AgiTool("Waits for an element to be visible on the page.")
    public String waitForElement(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The CSS selector of the element.") String cssSelector,
            @AgiToolParam("The maximum time to wait in seconds.") int timeoutSeconds) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(timeoutSeconds));
        wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(cssSelector)));
        return "Element '" + cssSelector + "' is now visible.";
    }

    /**
     * Executes arbitrary JavaScript in the current browser session.
     *
     * @param droneId The ID of the drone.
     * @param script The script.
     * @return The result.
     */
    @AgiTool("Executes arbitrary JavaScript in the current browser session.")
    public Object executeScript(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("The JavaScript code to execute.") String script) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        JavascriptExecutor js = (JavascriptExecutor) driver;
        return js.executeScript(script);
    }

    /**
     * Fills a web form with the provided data.
     *
     * @param droneId The ID of the drone.
     * @param data The form data.
     * @return A status message.
     */
    @AgiTool("Fills a web form with the provided data.")
    public String fillForm(
            @AgiToolParam("The ID of the drone.") String droneId,
            @AgiToolParam("A map of field IDs or Names to values.") Map<String, String> data) {
        WebDriver driver = getDriver(droneId);
        if (driver == null) {
            return "No active session.";
        }
        StringBuilder sb = new StringBuilder("Form filling results:\n");
        data.forEach((key, value) -> {
            try {
                WebElement el = null;
                try {
                    el = driver.findElement(By.id(key));
                } catch (Exception e) {
                }
                if (el == null) try {
                    el = driver.findElement(By.name(key));
                } catch (Exception e) {
                }
                if (el != null) {
                    el.clear();
                    el.sendKeys(value);
                    sb.append("- Filled '").append(key).append("'\n");
                } else {
                    sb.append("- Could not find field: ").append(key).append("\n");
                }
            } catch (Exception e) {
                sb.append("- Error filling '").append(key).append("': ").append(e.getMessage()).append("\n");
            }
        });
        return sb.toString();
    }

    /**
     * Closes the browser session.
     *
     * @param droneId The ID of the drone.
     * @return A confirmation message.
     */
    @AgiTool("Closes the browser session.")
    public String close(@AgiToolParam("The ID of the drone.") String droneId) {
        ChromeDrone d = drones.get(droneId);
        if (d != null && d.driver != null) {
            d.driver.quit();
            d.driver = null;
            d.port = -1;
            drones.remove(droneId);
            return "Browser session closed for drone: " + droneId;
        }
        return "No active session to close for drone: " + droneId;
    }

    /**
     * Gets the active WebDriver instance for a drone.
     *
     * @param droneId The drone ID.
     * @return The WebDriver.
     */
    public synchronized WebDriver getDriver(String droneId) {
        ChromeDrone d = drones.get(droneId);
        if (d == null) {
            return null;
        }
        if (d.driver != null) {
            try {
                d.currentUrl = d.driver.getCurrentUrl();
                return d.driver;
            } catch (Exception e) {
                log.warn("Drone '{}' unresponsive, attempting reconnection...", droneId);
                d.driver = null;
            }
        }
        if (d.port > 0 && !d.initializing) {
            connectToExistingInternal(d, d.port);
        }
        return d.driver;
    }

    // --- PRIVATE METHODS ---
    private synchronized void initDriver(ChromeDrone d, ChromeOptions options, Map<String, String> environment) {
        d.initializing = true;
        d.lastError = null;
        if (d.driver != null) {
            try {
                d.driver.quit();
            } catch (Exception e) {
            }
        }
        String driverPath = ChromeUtils.findChromeDriver();
        if (driverPath == null) {
            d.lastError = "ChromeDriver not found.";
            d.initializing = false;
            return;
        }
        System.setProperty("webdriver.chrome.driver", driverPath);
        try {
            CompletableFuture<WebDriver> future = CompletableFuture.supplyAsync(() -> {
                if (environment != null && !environment.isEmpty()) {
                    ChromeDriverService service = new ChromeDriverService.Builder().usingDriverExecutable(new File(driverPath)).usingAnyFreePort().withEnvironment(environment).build();
                    return new ChromeDriver(service, options);
                } else {
                    return new ChromeDriver(options);
                }
            }, getExecutorService());
            d.driver = future.get(60, TimeUnit.SECONDS);
            log("Drone '" + d.id + "' successfully initialized. URL: " + d.driver.getCurrentUrl());
        } catch (Exception e) {
            d.lastError = ExceptionUtils.getStackTrace(e);
            error("Failed to initialize drone '" + d.id + "':\n" + d.lastError);
        } finally {
            d.initializing = false;
        }
    }

    private String getProcessReport() {
        StringBuilder sb = new StringBuilder("- **Running Chrome Processes**:\n");
        boolean foundRunning = false;
        try {
            List<ProcessHandle> processes = ProcessHandle.allProcesses()
                    .filter(p -> p.info().command().map(c -> c.toLowerCase().contains("chrome")).orElse(false))
                    .toList();

            for (ProcessHandle p : processes) {
                String cmdLine = getCommandLine(p);
                if (cmdLine.contains("--type=") || cmdLine.contains("chrome-sandbox")) {
                    continue;
                }
                foundRunning = true;
                String port = extractArg(cmdLine, "--remote-debugging-port");
                String profile = extractArg(cmdLine, "--profile-directory");
                String dataDir = extractArg(cmdLine, "--user-data-dir");

                if (port != null && (port.equals("0") || port.isEmpty()) && dataDir != null) {
                    String detectedPort = detectPortFromFiles(dataDir, profile != null ? profile : "Default");
                    port = detectedPort != null ? detectedPort + " (Detected from file)" : "0 (Actual port unknown)";
                }
                boolean isManaged = dataDir != null && dataDir.contains("scoped_dir");

                sb.append("  - PID: ").append(p.pid());
                if (isManaged) {
                    sb.append(" [ORPHANED/MANAGED]");
                }
                if (port != null) {
                    sb.append(" [DEBUG MODE - Port: ").append(port).append("]");
                } else {
                    sb.append(" [STANDARD MODE]");
                }
                if (profile != null) {
                    sb.append(" [Profile: ").append(profile).append("]");
                }
                if (dataDir != null) {
                    sb.append(" [Data: ").append(dataDir).append("]");
                }
                p.info().startInstant().ifPresent(i -> sb.append(" [Started: ").append(i).append("]"));
                sb.append("\n");
            }
        } catch (Exception e) {
            sb.append("  - Error scanning processes: ").append(e.getMessage()).append("\n");
        }
        if (!foundRunning) {
            sb.append("  - None detected.\n");
        }
        return sb.toString();
    }

    private String getCommandLine(ProcessHandle p) {
        String cmd = p.info().commandLine().orElse("");
        if (cmd.isEmpty()) {
            if (SystemUtils.IS_OS_UNIX) {
                try {
                    Shell shell = getToolkit(Shell.class);
                    if (shell != null) {
                        ShellExecutionResult res = shell.runAndWait("ps -p " + p.pid() + " -o command=", Shell.ShellType.BASH, null);
                        if (res.getExitCode() == 0) {
                            return res.getStdOut().trim();
                        }
                    }
                } catch (Exception e) {
                }
            } else if (SystemUtils.IS_OS_WINDOWS) {
                try {
                    Shell shell = getToolkit(Shell.class);
                    if (shell != null) {
                        ShellExecutionResult res = shell.runAndWait("wmic process where ProcessId=" + p.pid() + " get CommandLine", Shell.ShellType.CMD, null);
                        if (res.getExitCode() == 0) {
                            String[] lines = res.getStdOut().split("\\r?\\n");
                            if (lines.length > 1) {
                                return lines[1].trim();
                            }
                        }
                    }
                } catch (Exception e) {
                }
            }
        }
        return cmd;
    }

    /**
     * Extracts the value of a command-line argument.
     *
     * @param cmdLine The full command line string.
     * @param argName The name of the argument to extract.
     * @return The argument value, or null if not found.
     */
    private String extractArg(String cmdLine, String argName) {
        Pattern p = Pattern.compile(argName + "[= ]\"?([^\"]+)\"?");
        Matcher m = p.matcher(cmdLine);
        if (m.find()) {
            return m.group(1);
        }
        return null;
    }

    /**
     * Attempts to detect the active DevTools port from the file system.
     *
     * @param userDataDir The Chrome user data directory.
     * @param profileDir The profile directory name.
     * @return The detected port, or null if not found.
     */
    private String detectPortFromFiles(String userDataDir, String profileDir) {
        try {
            File activePortFile = new File(new File(userDataDir, profileDir), "DevToolsActivePort");
            if (!activePortFile.exists()) {
                activePortFile = new File(userDataDir, "DevToolsActivePort");
            }
            if (activePortFile.exists()) {
                List<String> lines = Files.readAllLines(activePortFile.toPath());
                if (!lines.isEmpty()) {
                    return lines.get(0).trim();
                }
            }
        } catch (Exception e) {
        }
        return null;
    }

    /**
     * Detects the active Chrome profile by searching for lock files.
     *
     * @param userDataDir The Chrome user data directory.
     * @return The detected profile name, or 'Default' if none found.
     */
    private String detectActiveProfile(String userDataDir) {
        File dir = new File(userDataDir);
        if (!dir.exists() || !dir.isDirectory()) {
            return "Default";
        }
        Path rootLock = dir.toPath().resolve("SingletonLock");
        if (Files.exists(rootLock, LinkOption.NOFOLLOW_LINKS)) {
            try {
                Path target = Files.readSymbolicLink(rootLock);
                String targetStr = target.toString();
                int lastDash = targetStr.lastIndexOf('-');
                if (lastDash != -1) {
                    long pid = Long.parseLong(targetStr.substring(lastDash + 1));
                    Optional<ProcessHandle> ph = ProcessHandle.of(pid);
                    if (ph.isPresent()) {
                        String profile = extractArg(getCommandLine(ph.get()), "--profile-directory");
                        if (profile != null) {
                            return profile;
                        }
                    }
                }
            } catch (Exception e) {
            }
            return "Default";
        }
        File[] profiles = dir.listFiles(f -> f.isDirectory() && (f.getName().equals("Default") || f.getName().startsWith("Profile ")));
        if (profiles != null) {
            for (File p : profiles) {
                if (hasLock(p)) {
                    return p.getName();
                }
            }
        }
        return "Default";
    }

    /**
     * Checks if a directory contains any Chrome lock files.
     *
     * @param dir The directory to check.
     * @return True if a lock is found.
     */
    private boolean hasLock(File dir) {
        String[] lockNames = {"SingletonLock", "SingletonCookie", "SingletonSocket", "lock"};
        for (String name : lockNames) {
            if (Files.exists(dir.toPath().resolve(name), LinkOption.NOFOLLOW_LINKS)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Internal implementation of killAll.
     */
    private String killAllInternal() {
        long myPid = ProcessHandle.current().pid();
        try {
            List<ProcessHandle> toKill = ProcessHandle.allProcesses()
                    .filter(p -> p.pid() != myPid && p.info().command().orElse("").toLowerCase().matches(".*chrome.*|.*chromedriver.*"))
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
            return "Terminated " + count + " Chrome-related processes (Gentle shutdown attempted).";
        } catch (Exception e) {
            return "Error during cleanup: " + e.getMessage();
        }
    }

    /**
     * Surgically resets the 'exit_type' in the Chrome Preferences file to
     * 'Normal'.
     */
    private String resetExitStateInternal(String userDataDir, String profileDir) {
        File prefsFile = new File(new File(userDataDir != null ? userDataDir : ChromeUtils.getDefaultChromeUserDataDir(), profileDir != null ? profileDir : "Default"), "Preferences");
        if (!prefsFile.exists()) {
            return "Preferences file not found.";
        }
        try {
            String content = Files.readString(prefsFile.toPath(), StandardCharsets.UTF_8);
            String updated = content.replaceAll("\"exit_type\"\\s*:\\s*\"[^\"]+\"", "\"exit_type\":\"Normal\"")
                    .replaceAll("\"exited_cleanly\"\\s*:\\s*false", "\"exited_cleanly\":true");
            Files.writeString(prefsFile.toPath(), updated, StandardCharsets.UTF_8);
            return "Successfully reset exit state.";
        } catch (Exception e) {
            return "Failed to reset exit state: " + e.getMessage();
        }
    }

    /**
     * Deletes the 'SingletonLock' or 'lock' file from the Chrome user data
     * directory.
     */
    private String clearSingletonLockInternal(String userDataDir) {
        String effectiveDataDir = userDataDir != null ? userDataDir : ChromeUtils.getDefaultChromeUserDataDir();
        for (String name : new String[]{"SingletonLock", "lock"}) {
            try {
                Files.deleteIfExists(new File(effectiveDataDir, name).toPath());
            } catch (Exception e) {
            }
        }
        return "Cleared locks.";
    }

    private String launchProfileChromeInternal(ChromeDrone d, String initialUrl) {
        if (d.headless) {
            ChromeOptions options = new ChromeOptions();
            options.addArguments("--headless=new", "--no-sandbox", "--disable-dev-shm-usage", "--ignore-certificate-errors");
            if (d.userDataDir != null && !d.userDataDir.isEmpty()) {
                options.addArguments("--user-data-dir=" + d.userDataDir);
            }
            if (d.profile != null && !d.profile.isEmpty()) {
                options.addArguments("--profile-directory=" + d.profile);
            }
            initDriver(d, options, null);
            return d.driver != null ? "Headless drone '" + d.id + "' launched." : "Failed to launch headless drone.";
        }
        String effectiveDataDir = d.userDataDir;
        if (effectiveDataDir.equals(ChromeUtils.getDefaultChromeUserDataDir())) {
            effectiveDataDir += "-anahata";
        }
        int debugPort = 9222;
        try {
            String chromeBinary = SystemUtils.IS_OS_MAC ? "\"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome\"" : (SystemUtils.IS_OS_WINDOWS ? "start chrome" : "google-chrome");
            String cmd = String.format("%s --user-data-dir=\"%s\" --profile-directory=\"%s\" --remote-allow-origins=* --disable-dev-shm-usage --remote-debugging-port=%d --remote-debugging-address=127.0.0.1 --new-window --restore-last-session --no-first-run --no-default-browser-check --disable-features=InProductHelp --disable-component-update --disable-default-apps --disable-blink-features=AutomationControlled --disable-extensions", chromeBinary, effectiveDataDir, d.profile, debugPort);
            if (initialUrl != null) {
                cmd += " \"" + initialUrl + "\"";
            }
            Shell shell = getToolkit(Shell.class);
            if (shell != null && SystemUtils.IS_OS_UNIX) {
                shell.runAndWait("nohup " + cmd + " > /dev/null 2>&1 &", Shell.ShellType.BASH, null);
            } else {
                Runtime.getRuntime().exec(new String[]{"bash", "-c", "nohup " + cmd + " > /dev/null 2>&1 &"});
            }
            Thread.sleep(3000);
            return connectToExistingInternal(d, debugPort);
        } catch (Exception e) {
            d.lastError = ExceptionUtils.getStackTrace(e);
            return "Failed to launch native Chrome: " + e.getMessage();
        }
    }

    private String connectToExistingInternal(ChromeDrone d, int port) {
        ChromeOptions options = new ChromeOptions();
        options.setExperimentalOption("debuggerAddress", "127.0.0.1:" + port);
        initDriver(d, options, null);
        if (d.driver != null) {
            d.port = port;
            return "Connected drone '" + d.id + "' to Chrome on port " + port + ". URL: " + d.driver.getCurrentUrl();
        }
        return "Failed to connect drone '" + d.id + "' to port " + port;
    }
}
