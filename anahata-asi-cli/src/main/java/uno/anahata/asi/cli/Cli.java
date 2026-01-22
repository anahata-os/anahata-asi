package uno.anahata.asi.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.InputUserMessage;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.model.provider.AbstractModel;

/**
 * The reusable, provider-agnostic core of the Anahata AI Command Line Interface.
 * This class encapsulates the entire interactive user session but is decoupled
 * from the application's assembly, which is handled by a separate launcher module.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class Cli {

    private final Chat chat;

    /**
     * Runs the main application loop.
     */
    public void run() {
        try (Scanner scanner = new Scanner(System.in)) {
            // If a model was pre-selected via command-line, go straight to chat.
            if (chat.getSelectedModel() != null) {
                runChatLoop(scanner);
                // If the chat loop breaks (e.g., user types 'exit' or '/menu'),
                // we fall through to the main menu logic.
            }
            
            // Otherwise, show the main menu.
            List<? extends AbstractModel> models = chat.getAllModels();
            if (models.isEmpty()) {
                System.out.println("No models found from any registered providers. Exiting.");
                return;
            }
            runMainMenu(scanner, models);
        }
        System.out.println("\nAnahata AI CLI shutting down.");
    }

    private void runMainMenu(Scanner scanner, List<? extends AbstractModel> models) {
        CliConfigMenu configMenu = new CliConfigMenu(chat, scanner);
        
        while (true) {
            System.out.println("\n===== Main Menu =====");
            System.out.println("1. Chat with a Model");
            System.out.println("2. Configure Chat");
            System.out.println("3. Exit");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine();

            switch (choice) {
                case "1":
                    selectModelAndChat(scanner, models);
                    break;
                case "2":
                    configMenu.runConfigMenu();
                    break;
                case "3":
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    private void selectModelAndChat(Scanner scanner, List<? extends AbstractModel> models) {
        System.out.println("\nAvailable Models for Chat:");
        for (int i = 0; i < models.size(); i++) {
            AbstractModel model = models.get(i);
            System.out.printf("%d: %s (%s) - Provider: %s\n", i + 1, model.getDisplayName(), model.getModelId(), model.getProviderId());
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

        chat.setSelectedModel(models.get(modelIndex));
        runChatLoop(scanner);
    }

    private void runChatLoop(Scanner scanner) {
        System.out.println("\nStarting chat with '" + chat.getSelectedModel().getDisplayName() + "'. Type 'exit', 'quit', or '/menu' to return to the menu.");

        while (true) {
            System.out.print("\nYou: ");
            String userInput = scanner.nextLine();

            if ("exit".equalsIgnoreCase(userInput) || "quit".equalsIgnoreCase(userInput)) {
                break;
            }
            
            if ("/menu".equalsIgnoreCase(userInput)) {
                break; // Exit the chat loop to return to the main menu
            }

            InputUserMessage userMessage = new InputUserMessage(chat);
            userMessage.setText(userInput);

            // Track history size before sending to identify new messages
            int historySizeBefore = chat.getContextManager().getHistory().size();
            
            System.out.println("Model: ...");
            chat.sendMessage(userMessage);
            
            // Display all new messages added during the turn (Model and Tool responses)
            List<AbstractMessage> history = chat.getContextManager().getHistory();
            for (int i = historySizeBefore; i < history.size(); i++) {
                AbstractMessage msg = history.get(i);
                if (msg instanceof UserMessage && !(msg instanceof InputUserMessage)) {
                    // Skip internal UserMessages (like RAG) to keep CLI clean
                    continue;
                }
                System.out.println("\n--- " + msg.getRole() + " (" + msg.getFrom() + ") ---");
                System.out.println(msg.asText(true));
            }

            chat.getLastResponse().ifPresent(response -> {
                System.out.println("\n[Finish Reason: " + response.getCandidates().get(0).getFinishReason() 
                        + ", Total Tokens: " + response.getTotalTokenCount() + "]");
            });
        }
    }
}
