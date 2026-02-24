/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.internal.OsUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.toolkit.shell.Shell;
import uno.anahata.asi.toolkit.shell.ShellExecutionResult;

/**
 * A toolkit for web automation and form filling using Selenium.
 * <p>
 * This toolkit supports acting as the user by launching Chrome with their 
 * existing profile or connecting to an already running instance.
 * </p>
 * <p>
 * <b>Note on Persistence:</b> The {@code driver} and {@code initializing} fields 
 * are marked as {@code transient} because they represent active runtime state 
 * that cannot be serialized. The {@code lastConnectedPort} is persisted to 
 * allow auto-reconnection during {@link #rebind()}.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@AiToolkit("A toolkit for web automation and form filling using Selenium.")
public class Browser extends AnahataToolkit implements Rebindable {

    /** The active WebDriver instance. */
    private transient WebDriver driver;
    
    /** The last error encountered during driver operations, including full stack trace. */
    private String lastError;
    
    /** Flag indicating if a background launch is in progress. */
    private transient boolean initializing = false;

    /** The last port used for a successful connection. Persisted for rebind. */
    private int lastConnectedPort = -1;

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        log.info("Rebinding Browser toolkit. Connection will be restored lazily on next tool call. Last known port: {}", lastConnectedPort);
        this.driver = null;
        this.initializing = false;
    }

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
               "## Chrome Toolkit Instructions:\n" +
               "- **Connection Protocol**: Use the `connect()` tool as your primary entry point. It automatically detects running browsers and handles the restart protocol if necessary.\n" +
               "- **Profile Awareness**: Always prefer the user's active profile (detected via `connect()`) to ensure access to their tabs and history.\n" +
               "- **Advanced Automation**: For complex tasks or Selenium features not exposed via standard tools, you can use `Java` or `NbJava` toolkits. Access the active driver by calling `getToolkit(Browser.class).getDriver()` from your compiled code."
        );
    }

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder("### Browser Environment\n");
        
        // 1. Connection Status
        sb.append("- **Status**: ").append(initializing ? "Initializing..." : (driver != null ? "CONNECTED" : "DISCONNECTED")).append("\n");
        if (lastConnectedPort > 0) {
            sb.append("- **Last Known Port**: ").append(lastConnectedPort).append("\n");
        }

        // 2. Available Profiles
        String userDataDir = OsUtils.getDefaultChromeUserDataDir();
        sb.append("- **Detected User Data Dir**: ").append(userDataDir).append("\n");
        
        File dir = new File(userDataDir);
        if (dir.exists() && dir.isDirectory()) {
            File[] profiles = dir.listFiles(f -> {
                return f.isDirectory() && (f.getName().equals("Default") || f.getName().startsWith("Profile "));
            });
            if (profiles != null && profiles.length > 0) {
                sb.append("- **Available Chrome Profiles**:\n");
                for (File p : profiles) {
                    sb.append("  - ").append(p.getName()).append("\n");
                }
            } else {
                sb.append("- **Available Chrome Profiles**: None found in directory.\n");
            }
        } else {
            sb.append("- **Available Chrome Profiles**: Directory does not exist or is not accessible.\n");
        }
        
        // 3. Detect Running Browsers
        sb.append(getProcessReport());
        
        if (lastError != null) {
            sb.append("- **Last Error**: ").append(lastError).append("\n");
        }
        
        if (driver != null) {
            try {
                sb.append("- **Active Session**: ").append(driver.getCurrentUrl()).append("\n");
            } catch (Exception e) {
                sb.append("- **Active Session**: Unresponsive\n");
            }
        }
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * High-level tool to connect to the user's browser.
     * <p>
     * This tool automatically detects running Chrome instances. If an instance 
     * is already in Debug Mode, it connects to it. If not, it performs a 
     * clean restart of the browser using the same profile but with remote 
     * debugging enabled.
     * </p>
     * 
     * @param profile An optional profile name to force (e.g., 'Profile 5'). If null, it will be auto-detected.
     * @return A status message.
     */
    @AiTool("Automatically connects to the user's browser, restarting it in debug mode if necessary.")
    public String connect(
            @AiToolParam("An optional profile name to force. If null, it will be auto-detected.") String profile
    ) {
        if (getDriver() != null) {
            try {
                return "Already connected to: " + driver.getCurrentUrl();
            } catch (Exception e) {
                log.warn("Existing driver unresponsive, attempting reconnection...");
            }
        }

        // 1. Scan for existing debuggable processes
        List<ProcessHandle> processes = ProcessHandle.allProcesses()
                .filter(p -> {
                    return p.info().command().map(c -> {
                        return c.toLowerCase().contains("chrome");
                    }).orElse(false);
                })
                .toList();

        String targetProfile = profile;
        String targetDataDir = OsUtils.getDefaultChromeUserDataDir();
        int debugPort = -1;

        for (ProcessHandle p : processes) {
            String cmdLine = getCommandLine(p);
            if (cmdLine.contains("--type=") || cmdLine.contains("chrome-sandbox")) {
                continue;
            }

            String portStr = extractArg(cmdLine, "--remote-debugging-port");
            String pDir = extractArg(cmdLine, "--profile-directory");
            String dDir = extractArg(cmdLine, "--user-data-dir");

            if (portStr != null && !portStr.equals("0")) {
                try {
                    debugPort = Integer.parseInt(portStr);
                    log("Found existing debuggable process (PID: " + p.pid() + ") on port " + debugPort);
                    break; 
                } catch (NumberFormatException e) {
                    // Ignore invalid port strings
                }
            }

            // Keep track of the most likely profile to restart
            if (targetProfile == null && pDir != null) {
                targetProfile = pDir;
            }
            if (dDir != null) {
                targetDataDir = dDir;
            }
        }

        if (debugPort != -1) {
            return connectToExistingInternal(debugPort);
        }

        // 2. No debuggable process found. Fallback to Lock-Hunter protocol for profile detection.
        if (targetProfile == null) {
            log("Process scan failed to identify profile. Initiating Lock-Hunter protocol...");
            targetProfile = detectActiveProfile(targetDataDir);
        }

        log("Initiating restart protocol for profile: '" + targetProfile + "' in '" + targetDataDir + "'");
        
        killAllInternal();
        clearSingletonLockInternal(targetDataDir);
        clearSingletonLockInternal(new File(targetDataDir, targetProfile).getAbsolutePath());
        resetExitStateInternal(targetDataDir, targetProfile);
        
        return launchProfileChromeInternal(targetDataDir, targetProfile, null);
    }

    /**
     * Gets the current status of the browser driver.
     * 
     * @return A status report.
     */
    @AiTool("Gets the current status of the browser driver.")
    public String getStatus() {
        if (initializing) {
            return "Browser is currently initializing...";
        }
        if (getDriver() == null) {
            return "No active browser session. Last error:\n" + (lastError != null ? lastError : "None");
        }
        try {
            return "Connected to: " + driver.getCurrentUrl();
        } catch (Exception e) {
            return "Driver is present but unresponsive: " + e.getMessage();
        }
    }

    /**
     * Terminates all running Chrome and ChromeDriver processes on the host system.
     * <p>
     * This tool uses a two-phase shutdown: it first sends a gentle termination 
     * signal to allow Chrome to save its state, and then forcibly destroys any 
     * remaining processes after a short delay.
     * </p>
     * 
     * @return A status message.
     */
    @AiTool("Terminates all running Chrome and ChromeDriver processes on the host system.")
    public String killAll() {
        return killAllInternal();
    }

    /**
     * Navigates the current browser session to a new URL.
     * 
     * @param url The URL to navigate to.
     * @return A status message.
     */
    @AiTool("Navigates the current browser session to a new URL.")
    public String navigate(@AiToolParam("The URL to navigate to.") String url) {
        if (getDriver() == null) {
            return "No active session.";
        }
        driver.get(url);
        return "Navigated to: " + url;
    }

    /**
     * Takes a screenshot of the current page and attaches it to the session.
     * 
     * @param name The name of the screenshot file (without extension).
     * @return A status message.
     */
    @AiTool("Takes a screenshot of the current page and attaches it to the session.")
    public String getScreenshot(@AiToolParam("The name of the screenshot file.") String name) throws Exception {
        if (getDriver() == null) {
            return "No active session.";
        }
        
        File srcFile = ((TakesScreenshot) driver).getScreenshotAs(OutputType.FILE);
        File screenshotDir = AsiContainer.getWorkDirSubDir("screenshots").toFile();
        File destFile = new File(screenshotDir, name + ".png");
        
        Files.copy(srcFile.toPath(), destFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        addAttachment(destFile);
        
        return "Screenshot '" + name + "' attached to session.";
    }

    /**
     * Lists all open tabs/windows in the current browser session.
     * <p>
     * This implementation uses the Chrome DevTools Protocol (CDP) to retrieve 
     * tab information without switching focus, which prevents window flickering 
     * and OS notifications.
     * </p>
     * 
     * @return A list of tab titles, URLs, and indices.
     */
    @AiTool("Lists all open tabs/windows in the current browser session.")
    public List<String> listTabs() {
        if (getDriver() == null) {
            return Collections.singletonList("No active session.");
        }
        
        List<String> handles = new ArrayList<>(driver.getWindowHandles());
        String current = null;
        try {
            current = driver.getWindowHandle();
        } catch (Exception e) {
            // Current window might have been closed
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
                    // Use CDP to get target info without switching focus
                    Map<String, Object> params = Map.of("targetId", handle);
                    Map<String, Object> result = chromeDriver.executeCdpCommand("Target.getTargetInfo", params);
                    Map<String, Object> targetInfo = (Map<String, Object>) result.get("targetInfo");
                    if (targetInfo != null) {
                        title = (String) targetInfo.get("title");
                        url = (String) targetInfo.get("url");
                        cdpSuccess = true;
                    }
                } catch (Exception e) {
                    log.debug("CDP Target.getTargetInfo failed for handle: " + handle, e);
                }
            }
            
            if (!cdpSuccess) {
                // Fallback to standard method (causes flickering)
                try {
                    driver.switchTo().window(handle);
                    switched = true;
                    title = driver.getTitle();
                    url = driver.getCurrentUrl();
                } catch (Exception e) {
                    title = "[Error accessing tab: " + e.getMessage() + "]";
                }
            }
            
            String marker = (current != null && handle.equals(current)) ? " [CURRENT]" : "";
            tabs.add(i + ": " + title + " (" + url + ")" + marker);
        }
        
        // CRITICAL FIX: Only switch back if we actually switched away. 
        // This prevents the browser from stealing focus if CDP worked.
        if (switched && current != null) {
            try {
                driver.switchTo().window(current);
            } catch (Exception e) {
                // Ignore
            }
        }
        
        return tabs;
    }

    /**
     * Switches the active tab/window by its index.
     * 
     * @param index The index of the tab (from listTabs).
     * @return A status message.
     */
    @AiTool("Switches the active tab/window by its index.")
    public String switchToTab(@AiToolParam("The index of the tab.") int index) {
        if (getDriver() == null) {
            return "No active session.";
        }
        
        List<String> handles = new ArrayList<>(driver.getWindowHandles());
        if (index < 0 || index >= handles.size()) {
            return "Invalid tab index: " + index + ". Total tabs: " + handles.size();
        }
        
        try {
            driver.switchTo().window(handles.get(index));
            return "Switched to tab: " + driver.getTitle() + " (" + driver.getCurrentUrl() + ")";
        } catch (Exception e) {
            return "Failed to switch to tab " + index + ": " + e.getMessage();
        }
    }

    /**
     * Navigates back in the browser history.
     * 
     * @return A status message.
     */
    @AiTool("Navigates back in the browser history.")
    public String goBack() {
        if (getDriver() == null) {
            return "No active session.";
        }
        driver.navigate().back();
        return "Navigated back.";
    }

    /**
     * Navigates forward in the browser history.
     * 
     * @return A status message.
     */
    @AiTool("Navigates forward in the browser history.")
    public String goForward() {
        if (getDriver() == null) {
            return "No active session.";
        }
        driver.navigate().forward();
        return "Navigated forward.";
    }

    /**
     * Refreshes the current page.
     * 
     * @return A status message.
     */
    @AiTool("Refreshes the current page.")
    public String refresh() {
        if (getDriver() == null) {
            return "No active session.";
        }
        driver.navigate().refresh();
        return "Page refreshed.";
    }

    /**
     * Gets the full HTML source of the current page.
     * 
     * @return The page source.
     */
    @AiTool("Gets the full HTML source of the current page.")
    public String getPageSource() {
        if (getDriver() == null) {
            return "No active session.";
        }
        return driver.getPageSource();
    }

    /**
     * Gets the visible text content of the current page.
     * 
     * @return The page text.
     */
    @AiTool("Gets the visible text content of the current page.")
    public String getPageText() {
        if (getDriver() == null) {
            return "No active session.";
        }
        return driver.findElement(By.tagName("body")).getText();
    }

    /**
     * Inspects the current page for input fields, textareas, selects, and buttons.
     * 
     * @return A summary of found elements.
     */
    @AiTool("Inspects the current page for input fields and buttons.")
    public String inspectForm() {
        if (getDriver() == null) {
            return "No active browser session.";
        }
        
        List<WebElement> inputs = driver.findElements(By.cssSelector("input, textarea, select"));
        StringBuilder sb = new StringBuilder("Found " + inputs.size() + " form elements:\n");
        for (WebElement input : inputs) {
            sb.append("- Tag: ").append(input.getTagName())
              .append(", Type: ").append(input.getAttribute("type"))
              .append(", Name: ").append(input.getAttribute("name"))
              .append(", ID: ").append(input.getAttribute("id"))
              .append(", Placeholder: ").append(input.getAttribute("placeholder"))
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
     * @param identifier The ID, Name, or visible text of the element.
     * @return A status message.
     */
    @AiTool("Clicks an element on the page.")
    public String clickElement(
            @AiToolParam("The ID, Name, or visible text of the element.") String identifier
    ) {
        if (getDriver() == null) {
            return "No active session.";
        }
        
        WebElement el = null;
        try {
            el = driver.findElement(By.id(identifier));
        } catch (Exception e) {
            // Ignore
        }
        if (el == null) {
            try {
                el = driver.findElement(By.name(identifier));
            } catch (Exception e) {
                // Ignore
            }
        }
        if (el == null) {
            try {
                el = driver.findElement(By.linkText(identifier));
            } catch (Exception e) {
                // Ignore
            }
        }
        if (el == null) {
            try {
                el = driver.findElement(By.xpath("//*[contains(text(), '" + identifier + "')]"));
            } catch (Exception e) {
                // Ignore
            }
        }
        
        if (el != null) {
            el.click();
            return "Clicked element: " + identifier;
        } else {
            return "Could not find element: " + identifier;
        }
    }

    /**
     * Scrolls the specified element into view.
     * 
     * @param identifier The ID, Name, or visible text of the element.
     * @return A status message.
     */
    @AiTool("Scrolls the specified element into view.")
    public String scrollToElement(@AiToolParam("The ID, Name, or visible text of the element.") String identifier) {
        if (getDriver() == null) {
            return "No active session.";
        }
        WebElement el = null;
        try {
            el = driver.findElement(By.id(identifier));
        } catch (Exception e) {
            // Ignore
        }
        if (el == null) {
            try {
                el = driver.findElement(By.name(identifier));
            } catch (Exception e) {
                // Ignore
            }
        }
        if (el == null) {
            try {
                el = driver.findElement(By.xpath("//*[contains(text(), '" + identifier + "')]"));
            } catch (Exception e) {
                // Ignore
            }
        }
        
        if (el != null) {
            ((JavascriptExecutor) driver).executeScript("arguments[0].scrollIntoView(true);", el);
            return "Scrolled to element: " + identifier;
        } else {
            return "Could not find element to scroll to: " + identifier;
        }
    }

    /**
     * Waits for an element to be visible on the page.
     * 
     * @param cssSelector The CSS selector of the element.
     * @param timeoutSeconds The maximum time to wait in seconds.
     * @return A status message.
     */
    @AiTool("Waits for an element to be visible on the page.")
    public String waitForElement(
            @AiToolParam("The CSS selector of the element.") String cssSelector,
            @AiToolParam("The maximum time to wait in seconds.") int timeoutSeconds
    ) {
        if (getDriver() == null) {
            return "No active session.";
        }
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(timeoutSeconds));
        wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(cssSelector)));
        return "Element '" + cssSelector + "' is now visible.";
    }

    /**
     * Executes arbitrary JavaScript in the current browser session.
     * 
     * @param script The JavaScript code to execute.
     * @return The result of the script execution.
     */
    @AiTool("Executes arbitrary JavaScript in the current browser session.")
    public Object executeScript(
            @AiToolParam("The JavaScript code to execute.") String script
    ) {
        if (getDriver() == null) {
            return "No active session.";
        }
        JavascriptExecutor js = (JavascriptExecutor) driver;
        return js.executeScript(script);
    }

    /**
     * Fills a web form with the provided data.
     * 
     * @param data A map of field identifiers (ID or Name) to values.
     * @return A status message.
     */
    @AiTool("Fills a web form with the provided data.")
    public String fillForm(
            @AiToolParam("A map of field IDs or Names to values.") Map<String, String> data
    ) {
        if (getDriver() == null) {
            return "No active session.";
        }
        
        StringBuilder sb = new StringBuilder("Form filling results:\n");
        data.forEach((key, value) -> {
            try {
                WebElement el = null;
                try {
                    el = driver.findElement(By.id(key));
                } catch (Exception e) {
                    // Ignore
                }
                if (el == null) {
                    try {
                        el = driver.findElement(By.name(key));
                    } catch (Exception e) {
                        // Ignore
                    }
                }
                
                if (el != null) {
                    el.clear();
                    el.sendKeys(value);
                    sb.append("- Filled '").append(key).append("'\n");
                } else {
                    sb.append("- Could not find field: ").append(key).append("\n");
                }
            } catch (Exception e) {
                sb.append("- Error filling '").append(key).append("': ").append(ExceptionUtils.getStackTrace(e)).append("\n");
            }
        });
        return sb.toString();
    }

    /**
     * Closes the browser session.
     * 
     * @return A confirmation message.
     */
    @AiTool("Closes the browser session.")
    public String close() {
        if (driver != null) {
            driver.quit(); 
            this.driver = null;
            this.lastConnectedPort = -1;
            return "Browser session closed.";
        }
        return "No active session to close.";
    }

    /**
     * Gets the active WebDriver instance, attempting to reconnect if necessary.
     * <p>
     * This method is public to allow advanced automation via {@code Java} or {@code NbJava} toolkits.
     * </p>
     * 
     * @return The WebDriver instance, or null if not connected.
     */
    public synchronized WebDriver getDriver() {
        if (driver != null) {
            try {
                // Quick health check
                driver.getCurrentUrl();
                return driver;
            } catch (Exception e) {
                log.warn("Existing driver unresponsive, attempting reconnection...");
                driver = null;
            }
        }

        if (lastConnectedPort > 0 && !initializing) {
            log.info("Lazy Reconnect: Attempting to restore connection to port {}", lastConnectedPort);
            connectToExistingInternal(lastConnectedPort);
        }

        return driver;
    }

    /* --- TROUBLESHOOTING TOOLS (Commented out to clean up API) ---
    
    @AiTool("Lists all Chrome profiles found in the user data directory.")
    public List<String> listProfiles() {
        String userDataDir = OsUtils.getDefaultChromeUserDataDir();
        File dir = new File(userDataDir);
        if (!dir.exists() || !dir.isDirectory()) {
            return Collections.singletonList("User data directory not found: " + userDataDir);
        }

        List<String> results = new ArrayList<>();
        results.add("User Data Dir: " + userDataDir);
        results.add("Root Lock: " + hasLock(dir));

        File[] profiles = dir.listFiles(f -> {
            return f.isDirectory() && (f.getName().equals("Default") || f.getName().startsWith("Profile "));
        });
        if (profiles != null) {
            for (File p : profiles) {
                results.add("- " + p.getName() + " [Lock: " + hasLock(p) + "]");
            }
        }
        return results;
    }

    @AiTool("Refreshes the list of running Chrome processes and their debug ports.")
    public String listProcesses() {
        return getProcessReport();
    }
    */

    // --- PRIVATE METHODS ---

    /**
     * Initializes the ChromeDriver with the specified options and environment.
     * 
     * @param options The ChromeOptions to use.
     * @param environment An optional map of environment variables for the driver process.
     */
    private synchronized void initDriver(ChromeOptions options, Map<String, String> environment) {
        initializing = true;
        lastError = null;
        if (driver != null) {
            try { 
                log("Quitting existing browser session...");
                driver.quit(); 
            } catch (Exception e) { 
                String stackTrace = ExceptionUtils.getStackTrace(e);
                log.error("Error quitting driver:\n{}", stackTrace);
                error("Error quitting existing driver:\n" + stackTrace);
            }
        }
        
        String driverPath = OsUtils.findChromeDriver();
        if (driverPath == null) {
            String error = "ChromeDriver not found in ~/bin or PATH. Please install it to use the Browser toolkit.";
            lastError = error;
            error(error);
            initializing = false;
            return;
        }
        
        System.setProperty("webdriver.chrome.driver", driverPath);
        log("Initializing ChromeDriver (" + driverPath + ") with options: " + options.toString());
        
        try {
            // Use the session's executor service to avoid ForkJoinPool restrictions
            CompletableFuture<WebDriver> future = CompletableFuture.supplyAsync(() -> {
                if (environment != null && !environment.isEmpty()) {
                    ChromeDriverService service = new ChromeDriverService.Builder()
                            .usingDriverExecutable(new File(driverPath))
                            .usingAnyFreePort()
                            .withEnvironment(environment)
                            .build();
                    return new ChromeDriver(service, options);
                } else {
                    return new ChromeDriver(options);
                }
            }, getExecutorService());
            
            this.driver = future.get(60, TimeUnit.SECONDS);
            log("ChromeDriver successfully initialized. Current URL: " + driver.getCurrentUrl());
        } catch (Exception e) {
            lastError = ExceptionUtils.getStackTrace(e);
            error("Failed to initialize ChromeDriver (Timeout or Error):\n" + lastError);
            log.error("Driver initialization failed", e);
        } finally {
            initializing = false;
        }
    }

    /**
     * Generates a detailed report of running Chrome processes.
     * 
     * @return A formatted string containing process information.
     */
    private String getProcessReport() {
        StringBuilder sb = new StringBuilder("- **Running Chrome Processes**:\n");
        boolean foundRunning = false;
        try {
            List<ProcessHandle> processes = ProcessHandle.allProcesses()
                    .filter(p -> {
                        return p.info().command().map(c -> {
                            return c.toLowerCase().contains("chrome");
                        }).orElse(false);
                    })
                    .toList();
            
            for (ProcessHandle p : processes) {
                String cmdLine = getCommandLine(p);
                // Skip helper processes to reduce noise
                if (cmdLine.contains("--type=") || cmdLine.contains("chrome-sandbox")) {
                    continue; 
                }
                
                foundRunning = true;
                String port = extractArg(cmdLine, "--remote-debugging-port");
                String profile = extractArg(cmdLine, "--profile-directory");
                String dataDir = extractArg(cmdLine, "--user-data-dir");
                
                // If port is 0 or missing but debugging is enabled, try to find the actual port from the file system
                if (port != null && (port.equals("0") || port.isEmpty()) && dataDir != null) {
                    String detectedPort = detectPortFromFiles(dataDir, profile != null ? profile : "Default");
                    if (detectedPort != null) {
                        port = detectedPort + " (Detected from file)";
                    } else {
                        port = "0 (Actual port unknown)";
                    }
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
                p.info().startInstant().ifPresent(i -> {
                    sb.append(" [Started: ").append(i).append("]");
                });
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

    /**
     * Reliable way to get the command line of a process, even on restricted systems.
     * 
     * @param p The process handle.
     * @return The full command line string.
     */
    private String getCommandLine(ProcessHandle p) {
        String cmd = p.info().commandLine().orElse("");
        if (cmd.isEmpty()) {
            if (SystemUtils.IS_OS_UNIX) {
                try {
                    Shell shell = getToolkit(Shell.class);
                    if (shell != null) {
                        ShellExecutionResult res = shell.runAndWait("ps -p " + p.pid() + " -o command=", Shell.ShellType.BASH);
                        if (res.getExitCode() == 0) {
                            return res.getStdOut().trim();
                        }
                    }
                } catch (Exception e) {
                    log.debug("Failed to get command line via ps for PID " + p.pid(), e);
                }
            } else if (SystemUtils.IS_OS_WINDOWS) {
                try {
                    Shell shell = getToolkit(Shell.class);
                    if (shell != null) {
                        // Use wmic to get the command line on Windows
                        ShellExecutionResult res = shell.runAndWait("wmic process where ProcessId=" + p.pid() + " get CommandLine", Shell.ShellType.CMD);
                        if (res.getExitCode() == 0) {
                            String out = res.getStdOut();
                            // Skip the header line
                            String[] lines = out.split("\\r?\\n");
                            if (lines.length > 1) {
                                return lines[1].trim();
                            }
                        }
                    }
                } catch (Exception e) {
                    log.debug("Failed to get command line via wmic for PID " + p.pid(), e);
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
        Pattern p = Pattern.compile(argName + "[= ]\"?([^\" ]+)\"?");
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
            // Chrome writes the port to DevToolsActivePort in the profile directory
            File profilePath = new File(userDataDir, profileDir);
            File activePortFile = new File(profilePath, "DevToolsActivePort");
            
            // Sometimes it's in the root data dir
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
            log.debug("Failed to read DevToolsActivePort", e);
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
        
        // Check the root for locks (often used by the 'Default' profile)
        Path rootLock = dir.toPath().resolve("SingletonLock");
        if (Files.exists(rootLock, LinkOption.NOFOLLOW_LINKS)) {
            log("Lock-Hunter: Found lock in root. Attempting to identify profile via PID...");
            try {
                Path target = Files.readSymbolicLink(rootLock);
                String targetStr = target.toString();
                int lastDash = targetStr.lastIndexOf('-');
                if (lastDash != -1) {
                    String pidStr = targetStr.substring(lastDash + 1);
                    long pid = Long.parseLong(pidStr);
                    Optional<ProcessHandle> ph = ProcessHandle.of(pid);
                    if (ph.isPresent()) {
                        String cmdLine = getCommandLine(ph.get());
                        String profile = extractArg(cmdLine, "--profile-directory");
                        if (profile != null) {
                            log("Lock-Hunter: Identified active profile via PID " + pid + ": " + profile);
                            return profile;
                        }
                    }
                }
            } catch (Exception e) {
                log.debug("Lock-Hunter: Failed to identify profile via root lock PID", e);
            }
            log("Lock-Hunter: Root lock found but PID identification failed. Assuming 'Default'.");
            return "Default"; 
        }

        File[] profiles = dir.listFiles(f -> {
            return f.isDirectory() && (f.getName().equals("Default") || f.getName().startsWith("Profile "));
        });
        if (profiles != null) {
            for (File p : profiles) {
                if (hasLock(p)) {
                    log("Lock-Hunter: Found lock in profile directory: " + p.getName());
                    return p.getName();
                }
            }
        }
        
        log("Lock-Hunter: No active locks found. Defaulting to 'Default'.");
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
            Path path = dir.toPath().resolve(name);
            // Use NOFOLLOW_LINKS to detect symlinks even if they are broken
            if (Files.exists(path, LinkOption.NOFOLLOW_LINKS)) {
                log("Lock-Hunter: Found lock file: " + path.toAbsolutePath());
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
                    .filter(p -> {
                        if (p.pid() == myPid) {
                            return false;
                        }
                        String cmd = p.info().command().orElse("").toLowerCase();
                        return cmd.contains("chrome") || cmd.contains("chromedriver");
                    })
                    .toList();
            
            int count = toKill.size();
            
            // Phase 1: Gentle termination
            toKill.forEach(ProcessHandle::destroy);
            
            // Wait up to 2 seconds for graceful exit
            for (int i = 0; i < 20; i++) {
                if (toKill.stream().noneMatch(ProcessHandle::isAlive)) {
                    break;
                }
                Thread.sleep(100);
            }
            
            // Phase 2: Forced destruction for survivors
            toKill.stream().filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly);
            
            this.driver = null;
            this.initializing = false;
            this.lastConnectedPort = -1;
            return "Terminated " + count + " Chrome-related processes (Gentle shutdown attempted).";
        } catch (Exception e) {
            return "Error during cleanup: " + e.getMessage();
        }
    }

    /**
     * Surgically resets the 'exit_type' in the Chrome Preferences file to 'Normal'.
     */
    private String resetExitStateInternal(String userDataDir, String profileDir) {
        String effectiveDataDir = userDataDir != null ? userDataDir : OsUtils.getDefaultChromeUserDataDir();
        String effectiveProfile = profileDir != null ? profileDir : "Default";
        
        File prefsFile = new File(new File(effectiveDataDir, effectiveProfile), "Preferences");
        if (!prefsFile.exists()) {
            return "Preferences file not found at: " + prefsFile.getAbsolutePath();
        }
        
        try {
            String content = Files.readString(prefsFile.toPath(), StandardCharsets.UTF_8);
            // Replace "exit_type":"Crashed" or "exit_type":"Abnormal" with "exit_type":"Normal"
            String updated = content.replaceAll("\"exit_type\"\\s*:\\s*\"[^\"]+\"", "\"exit_type\":\"Normal\"");
            updated = updated.replaceAll("\"exited_cleanly\"\\s*:\\s*false", "\"exited_cleanly\":true");
            
            Files.writeString(prefsFile.toPath(), updated, StandardCharsets.UTF_8);
            return "Successfully reset exit state for profile '" + effectiveProfile + "'.";
        } catch (Exception e) {
            return "Failed to reset exit state: " + e.getMessage();
        }
    }

    /**
     * Deletes the 'SingletonLock' or 'lock' file from the Chrome user data directory.
     */
    private String clearSingletonLockInternal(String userDataDir) {
        String effectiveDataDir = userDataDir != null ? userDataDir : OsUtils.getDefaultChromeUserDataDir();
        String[] lockNames = {"SingletonLock", "lock"};
        StringBuilder sb = new StringBuilder();
        
        for (String name : lockNames) {
            Path lockPath = new File(effectiveDataDir, name).toPath();
            try {
                if (Files.exists(lockPath, LinkOption.NOFOLLOW_LINKS)) {
                    Files.delete(lockPath);
                    sb.append("Successfully deleted ").append(name).append(" at: ").append(lockPath.toAbsolutePath()).append("\n");
                }
            } catch (Exception e) {
                sb.append("Failed to delete ").append(name).append(": ").append(e.getMessage()).append("\n");
            }
        }
        
        return sb.length() > 0 ? sb.toString() : "No lock files found in: " + effectiveDataDir;
    }

    /**
     * Launches a new Chrome instance using the user's actual profile.
     */
    private String launchProfileChromeInternal(String userDataDir, String profileDir, String initialUrl) {
        String effectiveDataDir = userDataDir != null ? userDataDir : OsUtils.getDefaultChromeUserDataDir();
        String effectiveProfile = profileDir != null ? profileDir : "Default";

        ChromeOptions options = new ChromeOptions();
        options.addArguments("--user-data-dir=" + effectiveDataDir);
        options.addArguments("--profile-directory=" + effectiveProfile);
        options.addArguments("--remote-allow-origins=*");
        options.addArguments("--no-sandbox");
        options.addArguments("--disable-dev-shm-usage");
        options.addArguments("--disable-gpu");
        options.addArguments("--remote-debugging-port=9222");
        options.addArguments("--remote-debugging-address=127.0.0.1");
        options.addArguments("--new-window");
        options.addArguments("--restore-last-session");
        options.addArguments("--no-first-run");
        options.addArguments("--no-default-browser-check");
        options.addArguments("--disable-features=InProductHelp");
        options.addArguments("--disable-component-update");
        options.addArguments("--disable-default-apps");
        options.addArguments("--disable-blink-features=AutomationControlled");
        options.addArguments("--disable-extensions");
        
        // Suppress the default "Chrome is being controlled by automated test software" infobar
        options.setExperimentalOption("excludeSwitches", Collections.singletonList("enable-automation"));
        options.setExperimentalOption("useAutomationExtension", false);
        
        // THE MAGIC BYPASS: Shift the HOME environment variable to trick Chrome's security check
        String fakeHome = System.getProperty("java.io.tmpdir") + File.separator + "anahata-fake-home";
        new File(fakeHome).mkdirs();
        Map<String, String> environment = Map.of("HOME", fakeHome);
        
        log("Starting ChromeDriver initialization (Synchronous with Home-Shift Bypass)...");
        initDriver(options, environment);
        
        if (driver != null) {
            this.lastConnectedPort = 9222;
            if (initialUrl != null) {
                log("Navigating to initial URL: " + initialUrl);
                driver.get(initialUrl);
            }
            return "Successfully connected to profile '" + effectiveProfile + "'. Current URL: " + driver.getCurrentUrl();
        } else {
            return "Failed to connect to profile '" + effectiveProfile + "'. Check 'Last Error' in RAG message.";
        }
    }

    /**
     * Connects to an existing Chrome instance running with remote debugging enabled.
     */
    private String connectToExistingInternal(int port) {
        ChromeOptions options = new ChromeOptions();
        options.setExperimentalOption("debuggerAddress", "127.0.0.1:" + port);
        initDriver(options, null);
        if (driver != null) {
            this.lastConnectedPort = port;
            return "Connected to Chrome on port " + port + ". Current URL: " + driver.getCurrentUrl();
        } else {
            return "Failed to connect to existing Chrome on port " + port;
        }
    }
}
