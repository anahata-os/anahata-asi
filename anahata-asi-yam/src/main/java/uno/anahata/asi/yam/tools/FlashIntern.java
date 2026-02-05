/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.yam.tools;

import java.io.File;
import java.nio.file.Files;
import java.util.concurrent.Callable;
/*
import uno.anahata.ai.Chat;
import uno.anahata.ai.swing.SwingChatConfig;
*/
import uno.anahata.asi.chat.Chat;

/**
 * The "Marketing Intern" utility. 
 * Uses Gemini 2.5 Flash to generate social media content from project state.
 */
public class FlashIntern implements Callable<String> {

    private static final String MODEL_ID = "models/gemini-2.5-flash";
    private static final String PROJECT_ROOT = "/home/pablo/NetBeansProjects/anahata-asi-parent";
    private static final String PLUGIN_ROOT = "/home/pablo/NetBeansProjects/anahata-netbeans-ai";

    @Override
    public String call() throws Exception {
        File ledgerFile = new File(PROJECT_ROOT, "ledger.md");
        File tasksFile = new File(PLUGIN_ROOT, "tasks.md");

        if (!ledgerFile.exists() || !tasksFile.exists()) {
            return "Error: Required files (ledger.md or tasks.md) not found.";
        }

        String ledgerContent = new String(Files.readAllBytes(ledgerFile.toPath()));
        String tasksContent = new String(Files.readAllBytes(tasksFile.toPath()));

        // Initialize a lightweight Chat instance for Flash
        /*
        SwingChatConfig config = new SwingChatConfig();
        config.getApi().setModelId(MODEL_ID);
        
        Chat intern = new Chat(config);
        
        String prompt = "You are the Anahata Marketing Intern. Your job is to generate a 'Social Strike Pack' " +
                "based on the current project status. Be enthusiastic, professional, and mention 'Visca el Barça!'\n\n" +
                "CURRENT LEDGER:\n" + ledgerContent + "\n\n" +
                "CURRENT TASKS:\n" + tasksContent + "\n\n" +
                "Please generate:\n" +
                "1. Three variations of a Tweet (X post).\n" +
                "2. One Reddit post for r/netbeans or r/java.\n" +
                "3. One LinkedIn update.\n" +
                "4. A short 'Hype' summary for the user.";

        return intern.sendText(prompt);
*/
        return null;
    }
}
