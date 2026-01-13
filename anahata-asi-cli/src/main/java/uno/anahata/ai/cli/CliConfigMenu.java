/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.cli;

import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;

/**
 * The main orchestrator for the Chat Configuration menu in the CLI.
 * This class delegates to specialized menu classes for specific configuration areas.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class CliConfigMenu {

    private final Chat chat;
    private final Scanner scanner;

    public void runConfigMenu() {
        // Instantiate sub-menus once per session
        SystemInstructionsMenu instructionsMenu = new SystemInstructionsMenu(chat, scanner);
        ToolkitsMenu toolkitsMenu = new ToolkitsMenu(chat, scanner);
        ResourcesMenu resourcesMenu = new ResourcesMenu(chat, scanner);

        while (true) {
            System.out.println("\n===== Chat Configuration =====");
            System.out.println("1. System Instructions Providers");
            System.out.println("2. Toolkits and Tools Overview");
            System.out.println("3. Managed Resources"); // New option
            System.out.println("4. Back to Main Menu");
            System.out.print("Enter your choice: ");

            String choice = scanner.nextLine();

            switch (choice) {
                case "1":
                    instructionsMenu.runMenu();
                    break;
                case "2":
                    toolkitsMenu.runMenu();
                    break;
                case "3":
                    resourcesMenu.runMenu();
                    break;
                case "4":
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        }
    }
}
