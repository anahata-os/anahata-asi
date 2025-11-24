package uno.anahata.ai.cli;

import uno.anahata.ai.Cli;

/**
 * Main entry point for the Anahata AI Command Line Interface.
 * <p>
 * This class acts as a simple launcher, delegating all functionality to the
 * generic {@link Cli} class in the anahata-ai core module. This design keeps
 * the CLI provider-agnostic, allowing it to dynamically load any available AI
 * provider at runtime.
 */
public class App {

    /**
     * The main method.
     *
     * @param args the command line arguments
     * @throws java.lang.Exception if an error occurs
     */
    public static void main(String[] args) throws Exception {
        Cli.main(args);
    }
}
