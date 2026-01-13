/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolParameter;
import uno.anahata.ai.model.tool.AbstractToolkit;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * Handles the CLI menu for managing Toolkits and viewing detailed tool information.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class ToolkitsMenu {

    private final Chat chat;
    private final Scanner scanner;

    public void runMenu() {
        List<AbstractToolkit<?>> toolkits = chat.getToolManager().getToolkits().values().stream().toList();

        while (true) {
            System.out.println("\n===== Toolkits Overview =====");
            for (int i = 0; i < toolkits.size(); i++) {
                AbstractToolkit<?> tk = toolkits.get(i);
                String status = tk.isEnabled() ? "ENABLED" : "DISABLED";
                System.out.printf("%d: [%s] %s - %s\n", i + 1, status, tk.getName(), tk.getDescription());
            }
            System.out.println("D: View Details of a Toolkit");
            System.out.println("T: Toggle Enable/Disable Toolkit");
            System.out.println("B: Back to Configuration Menu");
            System.out.print("Enter choice (1-" + toolkits.size() + ", D, T, B): ");

            String choice = scanner.nextLine().toUpperCase();

            if ("B".equals(choice)) {
                return;
            } else if ("T".equals(choice)) {
                System.out.print("Enter toolkit number to toggle: ");
                try {
                    int toolkitIndex = Integer.parseInt(scanner.nextLine()) - 1;
                    if (toolkitIndex >= 0 && toolkitIndex < toolkits.size()) {
                        AbstractToolkit<?> tk = toolkits.get(toolkitIndex);
                        tk.setEnabled(!tk.isEnabled());
                        System.out.printf("Toolkit '%s' is now %s.\n", tk.getName(), tk.isEnabled() ? "ENABLED" : "DISABLED");
                    } else {
                        System.out.println("Invalid toolkit number.");
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Invalid input. Please enter a number.");
                }
            } else if ("D".equals(choice)) {
                System.out.print("Enter toolkit number to view details: ");
                try {
                    int toolkitIndex = Integer.parseInt(scanner.nextLine()) - 1;
                    if (toolkitIndex >= 0 && toolkitIndex < toolkits.size()) {
                        displayToolkitDetails(toolkits.get(toolkitIndex));
                    } else {
                        System.out.println("Invalid toolkit number.");
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Invalid input. Please enter a number.");
                }
            } else {
                System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    private void displayToolkitDetails(AbstractToolkit<?> toolkit) {
        System.out.println("\n===== Toolkit Details: " + toolkit.getName() + " =====");
        System.out.println("Description: " + toolkit.getDescription());
        System.out.println("Default Retention: " + toolkit.getDefaultRetention() + " turns");
        System.out.println("Status: " + (toolkit.isEnabled() ? "ENABLED" : "DISABLED"));

        System.out.println("\n--- Tools ---");
        for (int i = 0; i < toolkit.getAllTools().size(); i++) {
            AbstractTool<?, ?> tool = toolkit.getAllTools().get(i);
            String permission = tool.getPermission().name();
            String status = tool.getPermission() == ToolPermission.DENY_NEVER ? "DENIED" : "ALLOWED";

            System.out.printf("\n%d. %s [%s, Retention: %d turns]\n", i + 1, tool.getName(), status, tool.getRetentionTurns());
            System.out.println("   Description: " + tool.getDescription());

            System.out.println("   Parameters:");
            if (tool.getParameters().isEmpty()) {
                System.out.println("     (None)");
            } else {
                for (AbstractToolParameter<?> param : tool.getParameters()) {
                    String required = param.isRequired() ? "REQUIRED" : "OPTIONAL";
                    System.out.printf("     - %s (%s): %s\n", param.getName(), required, param.getDescription());
                    System.out.println("       Schema:");
                    System.out.println("       " + JacksonUtils.prettyPrint(param.getJsonSchema()).replace("\n", "\n       "));
                }
            }

            System.out.println("   Return Type Schema:");
            if (tool.getResponseJsonSchema() == null) {
                System.out.println("     (Void)");
            } else {
                System.out.println("     " + JacksonUtils.prettyPrint(tool.getResponseJsonSchema()).replace("\n", "\n     "));
            }
        }
        System.out.println("\nPress ENTER to continue...");
        scanner.nextLine();
    }
}
