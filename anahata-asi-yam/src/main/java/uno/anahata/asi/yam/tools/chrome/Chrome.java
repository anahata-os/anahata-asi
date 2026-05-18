/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.chrome;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.toolkit.shell.Shell;
import uno.anahata.asi.toolkit.shell.ShellExecutionResult;
import uno.anahata.asi.yam.tools.browser.AbstractBrowser;
import uno.anahata.asi.yam.tools.browser.BrowserDrone;

/**
 * A toolkit for web automation and form filling using Chrome and Selenium.
 * <p>
 * This toolkit manages a fleet of {@link BrowserDrone} instances, supporting 
 * both headless execution and live connection to the user's active Chrome profiles.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for web automation and form filling using Chrome and Selenium.")
public class Chrome extends AbstractBrowser {

    /**
     * {@inheritDoc}
     * <p>Implementation details: Disables the toolkit by default to prevent 
     * accidental drone spawns in environments without local browsers.</p>
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    
    /**
     * {@inheritDoc}
     * <p>Provides context-specific instructions to the ASI regarding Chrome automation.</p>
     */
    @Override
    public List<String> getSystemInstructions() {
        return Collections.singletonList(
                "**Chrome Toolkit Instructions**:\n"
                + "- **Connection Protocol**: Use the `connect()` tool as your primary entry point. It automatically detects running browsers and handles the restart protocol if necessary.\n"
                + "- **Scraping Tips (Headless vs Visual)**: Use the visual `Chrome` toolkit to bypass strong bot protections (DataDome, Cloudflare) by hijacking the user's authenticated fingerprint.\n"
                + "- **Multi-Drone Routing**: All methods require a `droneId` to target the specific browser session.\n"
                + "- **Missing Binaries**: If Selenium cannot find the browser binary, use the `Shell` toolkit to locate it (e.g., `which google-chrome`) and pass the absolute path to the `binaryPath` parameter.\n"
                + "- **Extensibility**: The `Java` toolkit has `Selenium` and `Jsoup` on its classpath. Use it to cover any advanced gaps."
        );
    }

    /**
     * {@inheritDoc}
     * <p>Populates the RAG message with current Chrome environment state.</p>
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
            for (BrowserDrone d : drones.values()) {
                String state = d.initializing ? "Initializing" : (d.driver != null ? "Connected" : "Disconnected");
                if (d.lastError != null) {
                    state += " (Error)";
                }
                String url = d.currentUrl != null ? d.currentUrl : "N/A";
                sb.append(String.format("| %s | %d | %b | %s | %s | %s | %s |\n", 
                        d.id, d.port, d.headless, d.profile, d.userDataDir, state, url));
            }
        }
        String systemDefaultDir = ChromeUtils.getDefaultChromeUserDataDir();
        sb.append("\n- **System Default User Data Dir**: `").append(systemDefaultDir).append("`\n");

        File arsenalDir = AbstractAsiContainer.getWorkDirSubDir("yam/chrome").toFile();
        sb.append("\n### Chrome Toolkit Directory (Arsenal)\n");
        sb.append("**Path**: `").append(arsenalDir.getAbsolutePath()).append("`\n");
        if (arsenalDir.exists()) {
            File[] files = arsenalDir.listFiles();
            if (files != null && files.length > 0) {
                for (File f : files) {
                    if (f.isDirectory()) {
                        boolean isLocked = hasLock(f);
                        sb.append("- ").append(f.getName()).append(isLocked ? " [LOCKED]\n" : "\n");
                        File[] profiles = f.listFiles(p -> p.isDirectory() && (p.getName().equals("Default") || p.getName().startsWith("Profile ")));
                        if (profiles != null && profiles.length > 0) {
                            for (File p : profiles) {
                                sb.append("  - ").append(p.getName()).append("\n");
                            }
                        }
                    } else {
                        sb.append("- ").append(f.getName()).append("\n");
                    }
                }
            } else {
                sb.append("*Directory is empty.*\n");
            }
        }
        sb.append("\n").append(getProcessReport());
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Scans the OS process table to find running 
     * Chrome instances bound to the requested {@code dataDir}. If a matching 
     * process is found in debug mode, it attaches to the existing port; 
     * otherwise, it launches a fresh instance with a dynamic debug port.</p>
     * @param droneId   A unique ID for this drone.
     * @param profile   An optional profile name (e.g., 'Default', 'Profile 1').
     * @param headless  Whether to launch in an invisible headless mode.
     * @param dataDir   The path to the user data directory.
     * @param binaryPath Optional absolute path to the chrome executable.
     * @return A status message confirming the connection.
     * @throws AgiToolException If the droneId is already in use.
     */
    @AgiTool("Connects to or launches a Chrome browser instance.")
    public String connect(
            @AgiToolParam(value = "A unique ID for this drone.", required = true) String droneId,
            @AgiToolParam(value = "An optional profile name to force. If null, auto-detected.", required = false) String profile,
            @AgiToolParam(value = "Whether to launch headless. Default false.", required = false) Boolean headless,
            @AgiToolParam(value = "Custom user data dir. If null, defaults to system.", required = false) String dataDir,
            @AgiToolParam(value = "Optional path to the Chrome binary.", required = false) String binaryPath) throws AgiToolException {
        
        if (drones.containsKey(droneId)) {
            throw new AgiToolException("Drone ID '" + droneId + "' already exists. Please choose a different ID or close it first.");
        }
        
        BrowserDrone drone = new BrowserDrone();
        drone.id = droneId;
        drone.profile = profile;
        drone.headless = (headless != null && headless);
        drone.binaryPath = binaryPath;

        File arsenalDir = AbstractAsiContainer.getWorkDirSubDir("yam/chrome").toFile();
        String defaultManagedDir = new File(arsenalDir, "default").getAbsolutePath();

        if (dataDir == null || dataDir.isBlank()) {
            drone.userDataDir = drone.headless ? null : defaultManagedDir;
        } else if (!dataDir.contains("/") && !dataDir.contains("\\")) {
            drone.userDataDir = new File(arsenalDir, dataDir).getAbsolutePath();
        } else {
            drone.userDataDir = dataDir;
        }

        if (drone.userDataDir != null && drone.userDataDir.equals(ChromeUtils.getDefaultChromeUserDataDir())) {
            drone.userDataDir = defaultManagedDir;
        }

        String effectiveDataDir = drone.userDataDir;

        if (!drone.headless) {
            List<ProcessHandle> processes = ProcessHandle.allProcesses()
                    .filter(p -> p.info().command().map(c -> c.toLowerCase().contains("chrome")).orElse(false))
                    .toList();
            int remotePort = -1;
            for (ProcessHandle p : processes) {
                String cmdLine = getCommandLine(p);
                if (cmdLine.contains("--type=") || cmdLine.contains("chrome-sandbox")) {
                    continue;
                }
                
                String processDataDir = extractArg(cmdLine, "--user-data-dir");
                if (processDataDir == null) {
                    processDataDir = ChromeUtils.getDefaultChromeUserDataDir();
                }
                
                // Chrome orchestrates multiple profiles under ONE master process bound to the user data dir.
                // We MUST reuse the remote port of the master process for this data dir.
                if (!effectiveDataDir.equals(processDataDir)) {
                    continue;
                }
                
                String portStr = extractArg(cmdLine, "--remote-debugging-port");
                if (portStr != null && !portStr.equals("0")) {
                    try {
                        remotePort = Integer.parseInt(portStr);
                        break;
                    } catch (NumberFormatException e) {
                        log.error("Failed to parse port from string: {}", portStr, e);
                    }
                }
            }
            if (remotePort != -1) {
                String res = connectToExistingInternal(drone, remotePort);
                if (drone.driver != null || drone.port > 0) drones.put(droneId, drone);
                return res;
            }
        }
        
        if (drone.profile == null) {
            drone.profile = detectActiveProfile(drone.userDataDir);
        }
        
        String res = launchProfileChromeInternal(drone, null);
        if (drone.driver != null || drone.port > 0) drones.put(droneId, drone);
        return res;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public String killAll() {
        drones.clear();
        return killAllInternal();
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Iterates over all window handles and uses 
     * the Chrome DevTools Protocol (CDP) {@code Target.getTargetInfo} command 
     * to retrieve titles and URLs without forcing window focus. Falls back 
     * to standard {@code switchTo().window()} if CDP fails.</p>
     * @param droneId The ID of the drone to inspect.
     * @return A list of strings describing each open tab.
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
                    log.warn("CDP getTargetInfo failed for handle {}, falling back to SwitchTo", handle, e);
                }
            }
            
            if (!cdpSuccess) {
                try {
                    driver.switchTo().window(handle);
                    switched = true;
                    title = driver.getTitle();
                    url = driver.getCurrentUrl();
                } catch (Exception e) {
                    log.error("Failed to switch to window handle {}", handle, e);
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
        if (d.port > 0 && !d.initializing) {
            connectToExistingInternal(d, d.port);
        }
        return d.driver;
    }

    // --- PRIVATE METHODS ---

    private synchronized void initDriver(BrowserDrone d, ChromeOptions options, Map<String, String> environment) {
        d.initializing = true;
        d.lastError = null;
        if (d.driver != null) {
            try {
                d.driver.quit();
            } catch (Exception e) {
                log.error("Error quitting previous driver for drone {}", d.id, e);
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
            
            d.driver = future.get(60, TimeUnit.SECONDS);
            log("Drone '" + d.id + "' successfully initialized. URL: " + d.driver.getCurrentUrl());
        } catch (Exception e) {
            d.lastError = ExceptionUtils.getStackTrace(e);
            error("Failed to initialize drone '" + d.id + "':\n" + d.lastError);
        } finally {
            d.initializing = false;
        }
    }

    /**
     * Generates a human-readable markdown report of all active Chrome 
     * processes on the host system. 
     * <p>Uses {@link ProcessHandle} and OS-specific command line extraction 
     * (via {@code /proc} on Linux or {@code wmic} on Windows) to identify 
     * ports and data directories.</p>
     * @return A markdown string containing process PIDs, modes, and command lines.
     */
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
                sb.append("\n      Cmd: ").append(cmdLine).append("\n");
            }
        } catch (Exception e) {
            log.error("Error scanning Chrome processes", e);
            sb.append("  - Error scanning processes: ").append(e.getMessage()).append("\n");
        }
        if (!foundRunning) {
            sb.append("  - None detected.\n");
        }
        return sb.toString();
    }

    /**
     * Attempts to resolve the full command line of a process. 
     * <p>On Linux, this reads directly from {@code /proc/[pid]/cmdline} for 
     * maximum fidelity. On other platforms or as a fallback, it uses 
     * Shell-based queries.</p>
     * @param p The process handle to inspect.
     * @return The full command line string.
     */
    private String getCommandLine(ProcessHandle p) {
        if (SystemUtils.IS_OS_UNIX) {
            try {
                File cmdlineFile = new File("/proc/" + p.pid() + "/cmdline");
                if (cmdlineFile.exists()) {
                    String raw = Files.readString(cmdlineFile.toPath(), StandardCharsets.UTF_8);
                    return raw.replace('\0', ' ').trim();
                }
            } catch (Exception e) {}
        }
        String cmd = p.info().commandLine().orElse("");
        if (cmd.isEmpty() || !cmd.contains(" ")) {
            if (SystemUtils.IS_OS_UNIX) {
                try {
                    Shell shell = getToolkit(Shell.class);
                    if (shell != null) {
                        ShellExecutionResult res = shell.runAndWait("ps -p " + p.pid() + " -o args=", Shell.ShellType.BASH, null);
                        if (res.getExitCode() == 0) {
                            return res.getStdOut().trim();
                        }
                    }
                } catch (Exception e) {
                    log.error("Failed to retrieve command line via Shell", e);
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
                    log.error("Failed to retrieve command line via Shell on Windows", e);
                }
            }
        }
        return cmd;
    }

    private String extractArg(String cmdLine, String argName) {
        int idx = cmdLine.indexOf(argName);
        if (idx == -1) return null;
        
        String remainder = cmdLine.substring(idx + argName.length());
        if (remainder.startsWith("=") || remainder.startsWith(" ")) {
            remainder = remainder.substring(1).trim();
            if (remainder.startsWith("\"")) {
                int endQuote = remainder.indexOf('\"', 1);
                if (endQuote != -1) {
                    return remainder.substring(1, endQuote);
                }
            } else if (remainder.startsWith("'")) {
                int endQuote = remainder.indexOf('\'', 1);
                if (endQuote != -1) {
                    return remainder.substring(1, endQuote);
                }
            } else {
                int nextFlag = remainder.indexOf(" --");
                if (nextFlag != -1) {
                    return remainder.substring(0, nextFlag).trim();
                }
                return remainder.trim();
            }
        }
        return null;
    }

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
            log.error("Failed to read DevToolsActivePort file", e);
        }
        return null;
    }

    private String detectActiveProfile(String userDataDir) {
        if (userDataDir == null) return "Default";
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
                log.error("Failed to read SingletonLock symlink", e);
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

    private boolean hasLock(File dir) {
        String[] lockNames = {"SingletonLock", "SingletonCookie", "SingletonSocket", "lock"};
        for (String name : lockNames) {
            if (Files.exists(dir.toPath().resolve(name), LinkOption.NOFOLLOW_LINKS)) {
                return true;
            }
        }
        return false;
    }

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
            log.error("Failed during killAll cleanup", e);
            return "Error during cleanup: " + e.getMessage();
        }
    }

    private String resetExitStateInternal(String userDataDir, String profileDir) {
        File prefsFile = new File(new File(userDataDir, profileDir != null ? profileDir : "Default"), "Preferences");
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
            log.error("Failed to reset exit state in Preferences file", e);
            return "Failed to reset exit state: " + e.getMessage();
        }
    }

    private String clearSingletonLockInternal(String userDataDir) {
        for (String name : new String[]{"SingletonLock", "lock"}) {
            try {
                Files.deleteIfExists(new File(userDataDir, name).toPath());
            } catch (Exception e) {
                log.error("Failed to delete lock file {}", name, e);
            }
        }
        return "Cleared locks.";
    }

    private String launchProfileChromeInternal(BrowserDrone d, String initialUrl) {
        if (d.headless) {
            ChromeOptions options = new ChromeOptions();
            options.addArguments("--headless=new", "--no-sandbox", "--disable-dev-shm-usage", "--ignore-certificate-errors");
            if (d.userDataDir != null && !d.userDataDir.isEmpty()) {
                options.addArguments("--user-data-dir=" + d.userDataDir);
            }
            if (d.profile != null && !d.profile.isEmpty()) {
                options.addArguments("--profile-directory=" + d.profile);
            }
            if (d.binaryPath != null && !d.binaryPath.isEmpty()) {
                options.setBinary(d.binaryPath);
            }
            initDriver(d, options, null);
            return d.driver != null ? "Headless drone '" + d.id + "' launched." : "Failed to launch headless drone.";
        }
        
        String effectiveDataDir = d.userDataDir;
        if (effectiveDataDir.equals(ChromeUtils.getDefaultChromeUserDataDir())) {
            effectiveDataDir += "-anahata";
        }
        
        int remotePort;
        try (java.net.ServerSocket socket = new java.net.ServerSocket(0)) {
            remotePort = socket.getLocalPort();
        } catch (Exception e) {
            log.warn("Failed to find a free port, falling back to 9222", e);
            remotePort = 9222;
        }

        try {
            String chromeBinary = d.binaryPath != null && !d.binaryPath.isEmpty() ? "\"" + d.binaryPath + "\"" : (SystemUtils.IS_OS_MAC ? "\"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome\"" : (SystemUtils.IS_OS_WINDOWS ? "start chrome" : "google-chrome"));
            String cmd = String.format("%s --user-data-dir=\"%s\" --profile-directory=\"%s\" --remote-allow-origins=* --disable-dev-shm-usage --remote-debugging-port=%d --remote-debugging-address=127.0.0.1 --new-window --restore-last-session --no-first-run --no-default-browser-check --disable-features=InProductHelp --disable-component-update --disable-default-apps --disable-blink-features=AutomationControlled --disable-extensions", chromeBinary, effectiveDataDir, d.profile, remotePort);
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
            return connectToExistingInternal(d, remotePort);
            
        } catch (Exception e) {
            d.lastError = ExceptionUtils.getStackTrace(e);
            log.error("Failed to launch native Chrome", e);
            return "Failed to launch native Chrome: " + e.getMessage();
        }
    }

    private String connectToExistingInternal(BrowserDrone d, int port) {
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