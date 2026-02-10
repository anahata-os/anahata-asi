/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.resource.ResourceManager;

/**
 * Handles the CLI menu for viewing and managing stateful resources.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class ResourcesMenu {

    /** The parent chat session. */
    private final Chat chat;
    /** The scanner for user input. */
    private final Scanner scanner;

    /**
     * Runs the interactive CLI menu for resource management.
     */
    public void runMenu() {
        ResourceManager resourceManager = chat.getResourceManager();
        List<AbstractResource<?, ?>> resources = resourceManager.getResources().stream().toList();

        while (true) {
            System.out.println("\n===== Managed Resources =====");
            if (resources.isEmpty()) {
                System.out.println("(No resources currently managed in context.)");
            } else {
                for (int i = 0; i < resources.size(); i++) {
                    AbstractResource<?, ?> r = resources.get(i);
                    String type = r.getClass().getSimpleName();
                    String name = r.getName();
                    System.out.printf("%d: [%s] %s\n", i + 1, type, name);
                }
            }
            
            System.out.println("D: View Details (Header + Content)");
            System.out.println("B: Back to Configuration Menu");
            System.out.print("Enter choice (1-" + resources.size() + ", D, B): ");

            String choice = scanner.nextLine().toUpperCase();

            if ("B".equals(choice)) {
                return;
            } else if ("D".equals(choice)) {
                System.out.print("Enter resource number to view details: ");
                try {
                    int resourceIndex = Integer.parseInt(scanner.nextLine()) - 1;
                    if (resourceIndex >= 0 && resourceIndex < resources.size()) {
                        displayResourceDetails(resources.get(resourceIndex));
                    } else {
                        System.out.println("Invalid resource number.");
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Invalid input. Please enter a number.");
                }
            } else {
                System.out.println("Invalid choice. Please try again.");
            }
        }
    }

    /**
     * Displays the full details (header and content) of a specific resource.
     * @param resource The resource to display.
     */
    private void displayResourceDetails(AbstractResource<?, ?> resource) {
        System.out.println("\n===== Resource Details: " + resource.getName() + " =====");
        
        System.out.println(resource.getHeader());
        try {
            Object content = resource.getContent();
            if (content != null) {
                System.out.println("\nContent:");
                System.out.println(content.toString());
            }
        } catch (Exception e) {
            System.err.println("Error retrieving resource content: " + e.getMessage());
        }
        
        System.out.println("\nPress ENTER to continue...");
        scanner.nextLine();
    }
}
