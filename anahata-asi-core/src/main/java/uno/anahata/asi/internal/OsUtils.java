/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

import java.io.File;
import org.apache.commons.lang3.SystemUtils;

/**
 * Utility class for OS-specific path detection and environment resolution.
 * <p>
 * This class leverages {@link SystemUtils} from Apache Commons Lang 3 for 
 * reliable OS detection.
 * </p>
 * 
 * @author anahata
 */
public class OsUtils {

    /**
     * Gets the default user data directory for Chrome based on the current OS.
     * 
     * @return The absolute path to the Chrome user data directory.
     */
    public static String getDefaultChromeUserDataDir() {
        String home = System.getProperty("user.home");
        if (SystemUtils.IS_OS_WINDOWS) {
            return home + "\\AppData\\Local\\Google\\Chrome\\User Data";
        } else if (SystemUtils.IS_OS_MAC) {
            return home + "/Library/Application Support/Google/Chrome";
        } else {
            // Linux/Unix: Check common locations
            String[] paths = {
                ".config/google-chrome",
                ".config/google-chrome-stable",
                ".config/chromium",
                ".config/chromium-browser"
            };
            
            for (String path : paths) {
                File dir = new File(home, path);
                if (dir.exists() && dir.isDirectory()) {
                    return dir.getAbsolutePath();
                }
            }

            // Fallback to the most likely standard location
            return new File(home, ".config/google-chrome").getAbsolutePath();
        }
    }

    /**
     * Gets the expected name of the ChromeDriver executable for the current OS.
     * 
     * @return The executable name (e.g., "chromedriver.exe" or "chromedriver").
     */
    public static String getChromeDriverExecutableName() {
        return SystemUtils.IS_OS_WINDOWS ? "chromedriver.exe" : "chromedriver";
    }
    
    /**
     * Attempts to find the ChromeDriver executable in common locations.
     * <p>
     * It checks the user's {@code ~/bin} directory first, then searches the 
     * system {@code PATH}.
     * </p>
     * 
     * @return The absolute path to the executable, or {@code null} if not found.
     */
    public static String findChromeDriver() {
        String name = getChromeDriverExecutableName();
        String home = System.getProperty("user.home");
        
        // 1. Check ~/bin
        File binDir = new File(home, "bin/" + name);
        if (binDir.exists()) {
            return binDir.getAbsolutePath();
        }
        
        // 2. Check PATH
        String path = System.getenv("PATH");
        if (path != null) {
            String separator = SystemUtils.IS_OS_WINDOWS ? ";" : ":";
            for (String dir : path.split(separator)) {
                File f = new File(dir, name);
                if (f.exists()) {
                    return f.getAbsolutePath();
                }
            }
        }
        
        return null;
    }
}
