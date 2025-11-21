package uno.anahata.ai.gemini;

import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.core.Request;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AiProviderRegistry;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.tool.ToolManager;

/**
 * A simple CLI test harness to demonstrate model discovery and interaction.
 * @author anahata
 */
public class Main {

    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        System.out.println("Starting Anahata AI CLI Test Harness...");

        AiConfig config = new AiConfig("Main");
        AiProviderRegistry registry = new AiProviderRegistry();
        ToolManager toolManager = new ToolManager(config);
        AbstractAiProvider geminiProvider = new GeminiAiProvider(config, toolManager);
        registry.registerProvider(geminiProvider);

        System.out.println("\nFetching available models from '" + geminiProvider.getProviderId() + "'...");
        List<? extends AbstractModel> models = geminiProvider.getModels();

        if (models.isEmpty()) {
            System.out.println("No models found. Exiting.");
            return;
        }
        
        System.out.println("Found " + models.size() + " models.");
        
        try (Scanner scanner = new Scanner(System.in)) {
            mainLoop(scanner, geminiProvider, models);
        }
        
        System.out.println("\nCLI Test Harness shutting down.");
    }

    private static void mainLoop(Scanner scanner, AbstractAiProvider provider, List<? extends AbstractModel> models) {
        while (true) {
            System.out.println("\n===== Main Menu =====");
            System.out.println("1. View Model Details");
            System.out.println("2. Chat with a Model");
            System.out.println("3. Exit");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine();

            switch (choice) {
                case "1":
                    viewModelDetails(scanner, models);
                    break;
                case "2":
                    chatWithModel(scanner, provider, models);
                    break;
                case "3":
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    private static void viewModelDetails(Scanner scanner, List<? extends AbstractModel> models) {
        int currentIndex = 0;
        while (true) {
            AbstractModel model = models.get(currentIndex);
            System.out.println("\n----------------------------------------");
            System.out.printf("Model %d of %d: %s\n", (currentIndex + 1), models.size(), model.getDisplayName());
            System.out.println("----------------------------------------");
            System.out.println("  ID                : " + model.getModelId());
            System.out.println("  Version           : " + model.getVersion());
            System.out.println("  Description       : " + model.getDescription());
            System.out.println("  Max Input Tokens  : " + model.getMaxInputTokens());
            System.out.println("  Max Output Tokens : " + model.getMaxOutputTokens());
            System.out.println("  Supported Actions : " + model.getSupportedActions());
            
            System.out.print("\nEnter 'n' for next, 'p' for previous, or 'q' to quit: ");
            String nav = scanner.nextLine().trim().toLowerCase();
            
            if ("n".equals(nav)) {
                currentIndex = (currentIndex + 1) % models.size();
            } else if ("p".equals(nav)) {
                currentIndex = (currentIndex - 1 + models.size()) % models.size();
            } else if ("q".equals(nav)) {
                break;
            }
        }
    }

    private static void chatWithModel(Scanner scanner, AbstractAiProvider provider, List<? extends AbstractModel> models) {
        System.out.println("\nAvailable Models for Chat:");
        for (int i = 0; i < models.size(); i++) {
            System.out.printf("%d: %s (%s)\n", i + 1, models.get(i).getDisplayName(), models.get(i).getModelId());
        }

        System.out.print("Select a model number: ");
        int modelIndex;
        try {
            modelIndex = Integer.parseInt(scanner.nextLine()) - 1;
            if (modelIndex < 0 || modelIndex >= models.size()) {
                System.out.println("Invalid model number.");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Invalid input. Please enter a number.");
            return;
        }

        AbstractModel selectedModel = models.get(modelIndex);
        System.out.println("\nStarting chat with '" + selectedModel.getDisplayName() + "'. Type 'exit' or 'quit' to return to the menu.");

        while (true) {
            System.out.print("\nYou: ");
            String userInput = scanner.nextLine();

            if ("exit".equalsIgnoreCase(userInput) || "quit".equalsIgnoreCase(userInput)) {
                break;
            }

            UserMessage userMessage = new UserMessage();
            userMessage.getParts().add(new TextPart(userInput));

            Request request = new Request(selectedModel, Collections.singletonList(userMessage), RequestConfig.builder().build());
            
            System.out.println("Model: ...");
            Response response = provider.generateContent(request);

            response.getCandidates().stream()
                .findFirst()
                .ifPresent(message -> System.out.println(message.asText()));
            
            System.out.println("[Finish Reason: " + response.getFinishReason() + ", Total Tokens: " + response.getTotalTokenCount() + "]");
        }
    }
}