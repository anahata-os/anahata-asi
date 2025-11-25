package uno.anahata.ai.standalone;

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.cli.App;
import uno.anahata.ai.gemini.GeminiCliChatConfig;

/**
 * The main entry point for the Anahata AI standalone application.
 * This class assembles the necessary components (config, provider, chat session)
 * and hands control over to the CLI application.
 * @author anahata-ai
 */
public class Main {
    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        System.out.println("Starting Anahata AI Standalone...");

        AiConfig appConfig = new AiConfig("AnahataStandalone");
        GeminiCliChatConfig chatConfig = new GeminiCliChatConfig(appConfig);
        
        // The ChatConfig now needs the provider to be explicitly added.
        chatConfig.getProviderClasses().add(uno.anahata.ai.gemini.GeminiAiProvider.class);
        
        Chat chat = new Chat(chatConfig);
        
        // The App will be refactored to accept the chat session.
        // App cliApp = new App(chat);
        // cliApp.run();
        
        System.out.println("Framework assembled. CLI logic to be implemented.");
    }
}
