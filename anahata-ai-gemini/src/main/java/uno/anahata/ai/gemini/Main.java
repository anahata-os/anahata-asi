package uno.anahata.ai.gemini;

/**
 * A simple main class in the gemini provider module that launches the core,
 * provider-agnostic command-line interface.
 * 
 * @author Anahata
 */
public class Main {
    public static void main(String[] args) {
        // Delegate directly to the core CLI's main method.
        // The CLI will use reflection to find the GeminiCliChatConfig in this module's
        // classpath, which in turn registers the GeminiAiProvider.
        uno.anahata.ai.Cli.main(args);
    }
}
