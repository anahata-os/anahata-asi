package uno.anahata.asi.desktop;

import uno.anahata.asi.desktop.swing.AsiDesktopMainPanel;
import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Taskbar;
import java.util.prefs.Preferences;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.desktop.swing.AsiDesktopAsiContainer;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * The main entry point for the Anahata AI standalone Swing application.
 *
 * @author anahata
 */
@Slf4j
public class Main {

    /**
     * Private constructor to prevent instantiation of this main entry class.
     */
    private Main() {
        // Utility class entry point
    }

    /**
     * The main entry point for the standalone Swing application.
     * <p>
     * This method performs the following initialization sequence:
     * </p>
     * <ol>
     * <li>Configures the SLF4J simple logger level.</li>
     * <li>Initializes the FlatLaf Light Look-and-Feel.</li>
     * <li>Instantiates the standalone ASI container.</li>
     * <li>Assembles and displays the primary {@code JFrame} on the Event
     * Dispatch Thread (EDT).</li>
     * <li>Configures a global uncaught exception handler for background
     * threads.</li>
     * </ol>
     *
     * @param args Command-line arguments (currently unused).
     */
    public static void main(String[] args) {
        System.setProperty("apple.laf.useScreenMenuBar", "true");
        System.setProperty("apple.awt.application.name", "Anahata ASI");
        System.setProperty("apple.awt.application.appearance", "system");
        System.setProperty("flatlaf.useWindowDecorations", "true");
        System.setProperty("flatlaf.menuBarEmbedded", "true");
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");

        log.info("Starting Anahata ASI Desktop...");

        try {
            String lafClassName = Preferences.userNodeForPackage(Main.class).get("laf", "com.formdev.flatlaf.FlatDarkLaf");
            UIManager.setLookAndFeel(lafClassName);
        } catch (Exception e) {
            log.error("Failed to initialize Look and Feel", e);
        }

        // Core application setup
        AsiDesktopAsiContainer container = new AsiDesktopAsiContainer();

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata ASI");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setPreferredSize(new Dimension(1200, 900));

            try {
                // Provide multiple icon sizes for better OS integration
                frame.setIconImages(IconUtils.getLogoImages());
            } catch (Exception e) {
                log.warn("Could not load frame icons", e);
            }

            try {
                if (Taskbar.isTaskbarSupported()) {
                    Taskbar taskbar = Taskbar.getTaskbar();
                    if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                        taskbar.setIconImage(IconUtils.getLogoImages().getLast());
                    }
                }
            } catch (Exception e) {
                log.warn("Could not set taskbar icon", e);
            }

            // Create the StandaloneMainPanel which manages multiple sessions
            AsiDesktopMainPanel mainPanel = new AsiDesktopMainPanel(container);
            frame.add(mainPanel, BorderLayout.CENTER);

            try {
                if (Desktop.isDesktopSupported()) {
                    Desktop desktop = Desktop.getDesktop();
                    if (desktop.isSupported(Desktop.Action.APP_PREFERENCES)) {
                        desktop.setPreferencesHandler(evt -> {
                            SwingUtilities.invokeLater(() -> mainPanel.showPreferences());
                        });
                    }
                }
            } catch (Exception e) {
                log.warn("Could not register native desktop handlers", e);
            }

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);

            // Start the panel after the frame is visible to ensure listeners are active
            mainPanel.start();
        });

        Thread.setDefaultUncaughtExceptionHandler((thread, thrwbl) -> {
            log.error("Uncaught exception in thread {}", thread.getName(), thrwbl);
        });
    }
}
