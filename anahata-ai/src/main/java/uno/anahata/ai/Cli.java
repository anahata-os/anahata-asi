/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai;

import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import uno.anahata.ai.config.ChatConfig;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.ModelMessage;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractModel;

/**
 * A simple, interactive command-line interface for testing the Anahata AI framework.
 * @author anahata-gemini-pro-2.5
 */
public class Cli {

    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        System.out.println("Starting Anahata AI CLI...");

        // The application needs a name for its config directory
        AiConfig appConfig = new AiConfig("AnahataCli");
        
        // The ChatConfig defines which provider adapters to load.
        // We will look for a GeminiCliChatConfig on the classpath.
        ChatConfig chatConfig = findChatConfig(appConfig);
        
        if (chatConfig == null) {
            System.err.println("FATAL: Could not find a 'GeminiCliChatConfig' class on the classpath.");
            System.err.println("Please create a class named 'GeminiCliChatConfig' that extends 'uno.anahata.ai.config.ChatConfig' in your provider's sources.");
            return;
        }

        Chat chat = new Chat(chatConfig);
        List<? extends AbstractModel> models = chat.getAllModels();

        if (models.isEmpty()) {
            System.out.println("No models found from any registered providers. Exiting.");
            return;
        }

        try (Scanner scanner = new Scanner(System.in)) {
            runMainLoop(scanner, chat, models);
        }

        System.out.println("\nAnahata AI CLI shutting down.");
    }

    private static void runMainLoop(Scanner scanner, Chat chat, List<? extends AbstractModel> models) {
        while (true) {
            System.out.println("\n===== Main Menu =====");
            System.out.println("1. Chat with a Model");
            System.out.println("2. Exit");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine();

            switch (choice) {
                case "1":
                    selectModelAndChat(scanner, chat, models);
                    break;
                case "2":
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    private static void selectModelAndChat(Scanner scanner, Chat chat, List<? extends AbstractModel> models) {
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

        // Set the selected model on the chat session
        chat.setSelectedModel(models.get(modelIndex));
        System.out.println("\nStarting chat with '" + chat.getSelectedModel().getDisplayName() + "'. Type 'exit' or 'quit' to return to the menu.");

        while (true) {
            System.out.print("\nYou: ");
            String userInput = scanner.nextLine();

            if ("exit".equalsIgnoreCase(userInput) || "quit".equalsIgnoreCase(userInput)) {
                break;
            }

            UserMessage userMessage = new UserMessage();
            userMessage.getParts().add(new TextPart(userInput));
            
            System.out.println("Model: ...");
            Response response = chat.sendMessage(userMessage);

            // The caller is now responsible for choosing a candidate and adding it to the history
            response.getCandidates().stream()
                .findFirst()
                .ifPresent(message -> {
                    System.out.println(message.asText());
                    if (message instanceof ModelMessage) {
                        chat.chooseCandidate((ModelMessage) message); // Add the chosen one to the history
                    }
                });
            
            System.out.println("[Finish Reason: " + response.getFinishReason() + ", Total Tokens: " + response.getTotalTokenCount() + "]");
        }
    }
    
    private static ChatConfig findChatConfig(AiConfig appConfig) {
        try {
            // This uses reflection to find the config from a provider module
            Class<?> clazz = Class.forName("uno.anahata.ai.gemini.GeminiCliChatConfig");
            return (ChatConfig) clazz.getConstructor(AiConfig.class).newInstance(appConfig);
        } catch (Exception e) {
            return null;
        }
    }
}
