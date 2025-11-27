package uno.anahata.ai.standalone;

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.cli.Cli;
import uno.anahata.ai.gemini.GeminiCliChatConfig;

/**
 * The main entry point for the Anahata AI standalone application.
 * This class assembles the necessary components (config, provider, chat session)
 * and hands control over to the CLI application.
 * @author anahata-ai
 */
public class CliMain {
    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        System.out.println("Starting Anahata AI Standalone...");

        AiConfig appConfig = new AiConfig("AnahataStandalone");
        GeminiCliChatConfig chatConfig = new GeminiCliChatConfig(appConfig);
        
        // The ChatConfig now needs the provider to be explicitly added.
        chatConfig.getProviderClasses().add(uno.anahata.ai.gemini.GeminiAiProvider.class);
        
        Chat chat = new Chat(chatConfig);
        
        // Check for a command-line argument to pre-select a model.
        if (args.length > 0) {
            String providerAndModelId = args[0];
            System.out.println("Attempting to select model from argument: " + providerAndModelId);
            
            int slashIndex = providerAndModelId.indexOf('/');
            
            if (slashIndex <= 0 || slashIndex == providerAndModelId.length() - 1) {
                System.out.println("Invalid model format. Expected 'providerId/modelId'.");
            } else {
                String providerId = providerAndModelId.substring(0, slashIndex);
                String modelId = providerAndModelId.substring(slashIndex + 1);
                
                chat.getProviders().stream()
                    .filter(p -> p.getProviderId().equals(providerId))
                    .findFirst()
                    .flatMap(provider -> provider.findModel(modelId))
                    .ifPresentOrElse(
                        chat::setSelectedModel,
                        () -> System.out.println("Model not found: " + providerAndModelId)
                    );
            }
        }
        
        // Instantiate the reusable CLI application and run it.
        Cli cliApp = new Cli(chat);
        cliApp.run();
    }
}
