/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.cli;

import java.util.List;
import java.util.Scanner;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.ContextProvider;
import uno.anahata.ai.context.AbstractContextProvider;

/**
 * Handles the CLI menu for managing System Instructions Providers.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class SystemInstructionsMenu {

    private final Chat chat;
    private final Scanner scanner;

    public void runMenu() {
        List<ContextProvider> providers = chat.getContextManager().getProviders();

        while (true) {
            System.out.println("\n===== System Instructions Providers =====");
            for (int i = 0; i < providers.size(); i++) {
                ContextProvider p = providers.get(i);
                String status = p.isEnabled() ? "ENABLED" : "DISABLED";
                System.out.printf("%d: [%s] %s - %s\n", i + 1, status, p.getName(), p.getDescription());
            }
            System.out.println("T: Toggle Enable/Disable");
            System.out.println("B: Back to Configuration Menu");
            System.out.print("Enter choice (1-" + providers.size() + ", T, B): ");

            String choice = scanner.nextLine().toUpperCase();

            if ("B".equals(choice)) {
                return;
            } else if ("T".equals(choice)) {
                System.out.print("Enter provider number to toggle: ");
                try {
                    int providerIndex = Integer.parseInt(scanner.nextLine()) - 1;
                    if (providerIndex >= 0 && providerIndex < providers.size()) {
                        ContextProvider p = providers.get(providerIndex);
                        p.setEnabled(!p.isEnabled());
                        System.out.printf("Provider '%s' is now %s.\n", p.getName(), p.isEnabled() ? "ENABLED" : "DISABLED");
                    } else {
                        System.out.println("Invalid provider number.");
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Invalid input. Please enter a number.");
                }
            } else {
                System.out.println("Invalid choice. Please try again.");
            }
        }
    }
}
