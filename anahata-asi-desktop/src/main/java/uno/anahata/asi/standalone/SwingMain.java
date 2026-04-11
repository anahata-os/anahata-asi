package uno.anahata.asi.standalone;

import uno.anahata.asi.standalone.swing.AsiDesktopMainPanel;
import com.formdev.flatlaf.FlatLightLaf;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.standalone.swing.AsiDesktopAsiContainer;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * The main entry point for the Anahata AI standalone Swing application.
 *
 * @author anahata
 */
@Slf4j
public class SwingMain {

    /**
     * The main entry point for the standalone Swing application.
     * <p>
     * This method performs the following initialization sequence:
     * <ol>
     *   <li>Configures the SLF4J simple logger level.</li>
     *   <li>Initializes the FlatLaf Light Look-and-Feel.</li>
     *   <li>Instantiates the standalone ASI container.</li>
     *   <li>Assembles and displays the primary {@code JFrame} on the Event Dispatch Thread (EDT).</li>
     *   <li>Configures a global uncaught exception handler for background threads.</li>
     * </ol>
     * </p>
     * @param args Command-line arguments (currently unused).
     */
    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        log.info("Starting Anahata AI Standalone UI...");

        try {
            UIManager.setLookAndFeel(new FlatLightLaf());
        } catch (Exception e) {
            log.error("Failed to initialize FlatLaf", e);
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

            // Create the StandaloneMainPanel which manages multiple sessions
            AsiDesktopMainPanel mainPanel = new AsiDesktopMainPanel(container);
            frame.add(mainPanel, BorderLayout.CENTER);

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
