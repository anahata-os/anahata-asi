package uno.anahata.ai.standalone;

import com.formdev.flatlaf.FlatLightLaf;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.cli.CommandLineArgs;
import uno.anahata.ai.swing.chat.MainPanel;
import uno.anahata.ai.swing.chat.SwingChatConfig;

/**
 * The main entry point for the Anahata AI standalone Swing application.
 *
 * @author pablo
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
        AiConfig appConfig = new AiConfig("AnahataStandalone");
        SwingChatConfig chatConfig = new SwingChatConfig(appConfig);
        chatConfig.getProviderClasses().add(uno.anahata.ai.gemini.GeminiAiProvider.class);
        
        // Create the initial chat session
        Chat chat = new Chat(chatConfig); 

        // Centralized argument parsing
        CommandLineArgs.parse(chat, args);
        
        log.info("After passing command line args, selected model: " + chat.getSelectedModel());

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Anahata AI Assistant");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setPreferredSize(new Dimension(1200, 900));

            // Create the MainPanel which manages multiple sessions
            MainPanel mainPanel = new MainPanel(chatConfig);
            mainPanel.start();
            
            // Focus the initial chat session
            mainPanel.focus(chat);
            
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