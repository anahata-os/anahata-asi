package uno.anahata.asi.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.chat.Chat;
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
            System.out.println("2. Configure Chat"); // New option
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

            System.out.println("Model: ...");
            chat.sendMessage(userMessage);
            
            /*

            List<? extends AbstractModelMessage> candidates = response.getCandidates();

            if (candidates.isEmpty()) {
                System.out.println("The model did not provide any response.");
            } else if (candidates.size() > 1) {
                System.out.println("The model provided multiple candidates. Please choose one:");
                for (int i = 0; i < candidates.size(); i++) {
                    System.out.println("\n--- Candidate " + (i + 1) + " ---");
                    System.out.println(candidates.get(i).asText(true));
                }

                int choice = -1;
                while (choice < 1 || choice > candidates.size()) {
                    System.out.print("Enter your choice (1-" + candidates.size() + "): ");
                    try {
                        choice = Integer.parseInt(scanner.nextLine());
                        if (choice < 1 || choice > candidates.size()) {
                            System.out.println("Invalid choice. Please try again.");
                        }
                    } catch (NumberFormatException e) {
                        System.out.println("Invalid input. Please enter a number.");
                    }
                }
                chat.chooseCandidate(candidates.get(choice - 1));
            } else {
                // Single candidate case: it was auto-selected by Chat.sendMessage().
                // We just need to display its content.
                System.out.println(candidates.get(0).asText(true));
            }


            
            // The chosen candidate (either auto-selected or user-selected) is now in the history.
            // The final output is handled by the Chat.chooseCandidate logic, which may trigger
            // tool execution and a subsequent model call. The CLI only needs to display the final status.

            System.out.println("[Finish Reason: " + response.getFinishReason() + ", Total Tokens: " + response.getTotalTokenCount() + "]");
*/
        }
    }
}
