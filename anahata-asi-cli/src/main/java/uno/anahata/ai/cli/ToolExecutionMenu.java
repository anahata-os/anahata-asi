/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.internal.ObjectSummarizer;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * Handles the interactive CLI menu for managing tool execution requests.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class ToolExecutionMenu {

    private final Chat chat;
    private final Scanner scanner;
    private final List<AbstractToolResponse> responses;

    /**
     * Runs the interactive menu for all pending tool responses.
     * @return true if the user chose to execute all pending tools and send the results back to the model.
     */
    public boolean runMenu() {
        if (responses.isEmpty()) {
            return false;
        }

        System.out.println("\n===== Tool Execution Required =====");
        
        while (true) {
            displaySummary();
            
            System.out.println("\nOptions:");
            System.out.println("A: Approve All Pending (Execute all PENDING tools and send results to model)");
            System.out.println("D: Deny All Pending (Mark all PENDING tools as NOT_EXECUTED)");
            System.out.println("V: View Details / Execute Single Tool / Set Preference");
            System.out.println("B: Back to Chat (Wait for next user input)");
            System.out.print("Enter choice (A, D, V, B): ");

            String choice = scanner.nextLine().toUpperCase();

            switch (choice) {
                case "A":
                    responses.stream()
                        .filter(r -> r.getStatus() == ToolExecutionStatus.PENDING)
                        .forEach(AbstractToolResponse::execute);
                    return true; // Signal to the CLI to continue the chat loop
                case "D":
                    responses.stream()
                        .filter(r -> r.getStatus() == ToolExecutionStatus.PENDING)
                        .forEach(r -> r.reject("Denied by user."));
                    return false; // Wait for user input
                case "V":
                    runDetailMenu();
                    break;
                case "B":
                    return false; // Wait for user input
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    private void displaySummary() {
        System.out.println("\n--- Tool Call Summary ---");
        for (int i = 0; i < responses.size(); i++) {
            AbstractToolResponse r = responses.get(i);
            String status = r.getStatus().name();
            String name = r.getCall().getToolName();
            String args = ObjectSummarizer.formatValue(r.getCall().getArgs(), 50, false);
            String permission = r.getCall().getTool().getPermission().name();
            
            System.out.printf("%d: [%s] %s(%s) - Permission: %s\n", i + 1, status, name, args, permission);
        }
    }
    
    private void runDetailMenu() {
        System.out.print("Enter tool number for details: ");
        int toolIndex;
        try {
            toolIndex = Integer.parseInt(scanner.nextLine()) - 1;
            if (toolIndex < 0 || toolIndex >= responses.size()) {
                System.out.println("Invalid tool number.");
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Invalid input. Please enter a number.");
            return;
        }
        
        AbstractToolResponse response = responses.get(toolIndex);
        
        while (true) {
            displayToolDetails(response);
            
            System.out.println("\nOptions:");
            System.out.println("E: Execute (Run this tool now)");
            System.out.println("P: Set Preference (A/D/ALWAYS/NEVER)");
            System.out.println("B: Back to Summary");
            System.out.print("Enter choice (E, P, B): ");
            
            String choice = scanner.nextLine().toUpperCase();
            
            switch (choice) {
                case "E":
                    response.execute();
                    System.out.println("Tool executed. Status: " + response.getStatus());
                    break;
                case "P":
                    setToolPreference(response);
                    break;
                case "B":
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }
    
    private void displayToolDetails(AbstractToolResponse response) {
        System.out.println("\n--- Tool Details: " + response.getCall().getToolName() + " ---");
        System.out.println("Status: " + response.getStatus());
        System.out.println("Execution Time: " + response.getExecutionTimeMillis() + "ms");
        System.out.println("Current Permission: " + response.getCall().getTool().getPermission().name());
        
        System.out.println("\nArguments (JSON):");
        System.out.println(JacksonUtils.prettyPrint(response.getCall().getArgs().toString()));
        
        System.out.println("\nResult:");
        if (response.getResult() != null) {
            System.out.println(ObjectSummarizer.formatValue(response.getResult(), 1024, true));
        } else if (response.getError() != null) {
            System.out.println("Error: " + response.getError());
        } else {
            System.out.println("(Not yet executed)");
        }
        
        if (!response.getLogs().isEmpty()) {
            System.out.println("\nLogs:");
            response.getLogs().forEach(log -> System.out.println("  - " + log));
        }
        
        if (!response.getAttachments().isEmpty()) {
            // FIX: Explicitly cast to AbstractPart to call asText()
            response.getAttachments().forEach(att -> System.out.println("  - " + ((AbstractPart) att).asText()));
        }
    }
    
    private void setToolPreference(AbstractToolResponse response) {
        System.out.print("Enter new preference (A/D/ALWAYS/NEVER): ");
        String prefInput = scanner.nextLine().toUpperCase();
        
        ToolPermission newPermission = switch (prefInput) {
            case "A" -> ToolPermission.PROMPT;

            case "ALWAYS" -> ToolPermission.APPROVE_ALWAYS;
            case "NEVER" -> ToolPermission.DENY_NEVER;
            default -> null;
        };
        
        if (newPermission != null) {
            // 1. Update the tool instance's permission
            response.getCall().getTool().setPermission(newPermission);
            
            // 2. Update the persistent preferences map
            chat.getConfig().getAsiConfig().getPreferences().getToolPermissions()
                .put(response.getCall().getToolName(), newPermission);
            
            // 3. Save the preferences to disk
            chat.getConfig().getAsiConfig().savePreferences();
            
            System.out.println("Preference set to: " + newPermission.name());
        } else {
            System.out.println("Invalid preference input.");
        }
    }
}
