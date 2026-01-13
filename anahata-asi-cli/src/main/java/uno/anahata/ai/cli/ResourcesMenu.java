/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.resource.AbstractResource;
import uno.anahata.ai.model.resource.TextFileResource;
import uno.anahata.ai.resource.ResourceManager;

/**
 * Handles the CLI menu for viewing and managing stateful resources.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class ResourcesMenu {

    private final Chat chat;
    private final Scanner scanner;

    public void runMenu() {
        ResourceManager resourceManager = chat.getResourceManager();
        List<AbstractResource> resources = resourceManager.getResources().stream().toList();

        while (true) {
            System.out.println("\n===== Managed Resources =====");
            if (resources.isEmpty()) {
                System.out.println("(No resources currently managed in context.)");
            } else {
                for (int i = 0; i < resources.size(); i++) {
                    AbstractResource r = resources.get(i);
                    String type = r.getClass().getSimpleName();
                    String name = r.getName();
                    String turns = r.getTurnsRemaining() == null ? "Permanent" : r.getTurnsRemaining().toString();
                    System.out.printf("%d: [%s] %s (Turns Left: %s)\n", i + 1, type, name, turns);
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

    private void displayResourceDetails(AbstractResource resource) {
        System.out.println("\n===== Resource Details: " + resource.getName() + " =====");
        
        // The TextFileResource's cache contains the full formatted output (header + content)
        // which is the easiest way to satisfy the user's request for "both headers and content".
        if (resource instanceof TextFileResource) {
            TextFileResource textResource = (TextFileResource) resource;
            try {
                // Force a reload to ensure the header is up-to-date before displaying
                textResource.reload(); 
                System.out.println(textResource.getCache());
            } catch (Exception e) {
                System.out.println("Error reloading resource: " + e.getMessage());
            }
        } else {
            // For other resource types, just print the basic info and toString
            System.out.println("Resource Type: " + resource.getClass().getSimpleName());
            System.out.println("ID: " + resource.getId());
            System.out.println("Refresh Policy: " + resource.getRefreshPolicy());
            System.out.println("Context Position: " + resource.getContextPosition());
            System.out.println("Turns Remaining: " + (resource.getTurnsRemaining() == null ? "Permanent" : resource.getTurnsRemaining()));
            System.out.println("\n--- Raw Object Details ---");
            System.out.println(resource.toString());
        }
        
        System.out.println("\nPress ENTER to continue...");
        scanner.nextLine();
    }
}
