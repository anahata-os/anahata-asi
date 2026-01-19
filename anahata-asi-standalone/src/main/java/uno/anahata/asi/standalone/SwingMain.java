package uno.anahata.asi.standalone;

import uno.anahata.asi.standalone.swing.StandaloneMainPanel;
import com.formdev.flatlaf.FlatLightLaf;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.cli.CommandLineArgs;
import uno.anahata.asi.standalone.swing.StandaloneAsiContainer;
import uno.anahata.asi.standalone.swing.StandaloneChatConfig;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * The main entry point for the Anahata AI standalone Swing application.
 *
 * @author anahata
 */
@Slf4j
public class SwingMain {

    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        log.info("Starting Anahata AI Standalone UI...");

        try {
            UIManager.setLookAndFeel(new FlatLightLaf());
        } catch (Exception e) {
            log.error("Failed to initialize FlatLaf", e);
        }

        // Core application setup
        StandaloneAsiContainer container = new StandaloneAsiContainer(args);
        
        // Create the initial chat session
        //Chat chat = new Chat(new StandaloneChatConfig(container)); 

        // Centralized argument parsing
        //CommandLineArgs.parse(chat, args);
        
        //log.info("After passing command line args, selected model: " + chat.getSelectedModel());

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
            StandaloneMainPanel mainPanel = new StandaloneMainPanel(container);
            mainPanel.start();
            
            // Focus the initial chat session
            //mainPanel.focus(chat);
            
            frame.add(mainPanel, BorderLayout.CENTER);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });

        Thread.setDefaultUncaughtExceptionHandler((thread, thrwbl) -> {
            log.error("Uncaught exception in thread {}", thread.getName(), thrwbl);
        });
    }
}
