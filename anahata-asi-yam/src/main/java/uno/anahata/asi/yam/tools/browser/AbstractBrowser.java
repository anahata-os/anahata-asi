/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools.browser;

import java.io.File;
import java.nio.file.Files;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.persistence.Rebindable;

/**
 * The abstract base toolkit for all web automation implementations.
 * <p>
 * This class centralizes all pure Selenium WebDriver interactions (DOM
 * querying, execution, navigation, and screenshots). Concrete implementations
 * (like Chrome or Firefox) must handle the browser-specific launch protocols,
 * DevTools connections, and process management.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public abstract class AbstractBrowser extends AnahataToolkit implements Rebindable {

    /** The centralized registry mapping drone IDs to their stateful objects. */
    protected final Map<String, BrowserDrone> drones = new ConcurrentHashMap<>();

    /**
     * Resolves the active WebDriver for the requested drone.
     * <p>
     * If the connection is broken or lost, this method should attempt to 
     * re-establish the connection to the underlying port.
     * </p>
     * 
     * @param droneId The unique ID of the drone.
     * @return The active WebDriver, or null if it cannot be resolved.
     */
    protected abstract WebDriver getDriver(String droneId);

    /**
     * Closes all active browser processes.
     * 
     * @return A status message describing the cleanup operations.
     */
    @AgiTool("Terminates all running browser processes on the host system.")
    public abstract String killAll();

    /**
     * Lists all open tabs/windows in the specified drone.
     * <p>
     * Implementations should ideally use native DevTools protocols to avoid
     * forcing window focus and causing screen flickering.
     * </p>
     * 
     * @param droneId The ID of the drone.
     * @return A list of formatted tab titles and URLs.
     */
    @AgiTool("Lists all open tabs/windows in the specified drone.")
    public abstract List<String> listTabs(@AgiToolParam("The ID of the drone.") String droneId);

    // --- UNIVERSAL SELENIUM TOOLS ---

    /**
     * Navigates the specified drone to a new URL.
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

        List<String> handles = new java.util.ArrayList<>(driver.getWindowHandles());
        if (index < 0 || index >= handles.size()) {
            return "Invalid tab index: " + index + ". Total tabs: " + handles.size();
        }

        driver.switchTo().window(handles.get(index));
        return "Switched drone '" + droneId + "' to tab: " + driver.getTitle();
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
            return "No active session for drone: " + droneId;
        }
        
        driver.navigate().back();
        return "Navigated drone '" + droneId + "' back.";
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
            return "No active session for drone: " + droneId;
        }
        
        driver.navigate().forward();
        return "Navigated drone '" + droneId + "' forward.";
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
            return "No active session for drone: " + droneId;
        }
        
        driver.navigate().refresh();
        return "Refreshed drone '" + droneId + "'.";
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
            return "No active session for drone: " + droneId;
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
            return "No active session for drone: " + droneId;
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
            return "No active session for drone: " + droneId;
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
            return "No active session for drone: " + droneId;
        }
        
        WebElement el = locateElementGracefully(driver, identifier);
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
            return "No active session for drone: " + droneId;
        }
        
        WebElement el = locateElementGracefully(driver, identifier);
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
            return "No active session for drone: " + droneId;
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
            return "No active session for drone: " + droneId;
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
            return "No active session for drone: " + droneId;
        }
        
        StringBuilder sb = new StringBuilder("Form filling results:\n");
        
        for (Map.Entry<String, String> entry : data.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            
            WebElement el = locateElementGracefully(driver, key);
            if (el != null) {
                el.clear();
                el.sendKeys(value);
                sb.append("- Filled '").append(key).append("'\n");
            } else {
                sb.append("- Could not find field: ").append(key).append("\n");
            }
        }
        
        return sb.toString();
    }

    /**
     * Closes the browser session and removes it from the registry.
     *
     * @param droneId The ID of the drone.
     * @return A confirmation message.
     */
    @AgiTool("Closes the browser session.")
    public String close(@AgiToolParam("The ID of the drone.") String droneId) {
        BrowserDrone d = drones.get(droneId);
        if (d != null) {
            if (d.driver != null) {
                d.driver.quit();
                d.driver = null;
            }
            d.port = -1;
            drones.remove(droneId);
            return "Browser session closed for drone: " + droneId;
        }
        return "No active session to close for drone: " + droneId;
    }

    /**
     * Helper method to safely locate elements without throwing raw Selenium exceptions.
     * <p>
     * This avoids empty try-catch blocks and adheres to the Fail Fast architecture 
     * by checking collection boundaries natively.
     * </p>
     * 
     * @param driver The WebDriver instance.
     * @param identifier The element identifier (ID, Name, Link Text, or XPath text).
     * @return The WebElement, or null if not found.
     */
    protected WebElement locateElementGracefully(WebDriver driver, String identifier) {
        List<WebElement> elements = driver.findElements(By.id(identifier));
        if (!elements.isEmpty()) {
            return elements.get(0);
        }
        
        elements = driver.findElements(By.name(identifier));
        if (!elements.isEmpty()) {
            return elements.get(0);
        }
        
        elements = driver.findElements(By.linkText(identifier));
        if (!elements.isEmpty()) {
            return elements.get(0);
        }
        
        elements = driver.findElements(By.xpath("//*[contains(text(), '" + identifier + "')]"));
        if (!elements.isEmpty()) {
            return elements.get(0);
        }
        
        return null;
    }
}