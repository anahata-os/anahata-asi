package uno.anahata.asi.standalone;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.standalone.swing.StandaloneAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.cli.Cli;
import uno.anahata.asi.gemini.GeminiCliAgiConfig;

/**
 * The main entry point for the Anahata AI standalone application.
 * This class assembles the necessary components (config, provider, agi session)
 * and hands control over to the CLI application.
 * @author anahata-ai
 */
public class CliMain {
    public static void main(String[] args) {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        System.out.println("Starting Anahata AI Standalone...");

        AsiContainer appConfig = new StandaloneAsiContainer(args);
        GeminiCliAgiConfig agiConfig = new GeminiCliAgiConfig(appConfig);
        
        // The AgiConfig now needs the provider to be explicitly added.
        agiConfig.getProviderClasses().add(uno.anahata.asi.gemini.GeminiAgiProvider.class);
        
        Agi agi = new Agi(agiConfig);
        
        // Check for a command-line argument to pre-select a model.
        if (args.length > 0) {
            String providerAndModelId = args[0];
            System.out.println("Attempting to select model from argument: " + providerAndModelId);
            
            int slashIndex = providerAndModelId.indexOf('/');
            
            if (slashIndex <= 0 || slashIndex == providerAndModelId.length() - 1) {
                System.out.println("Invalid model format. Expected 'providerId/modelId'.");
            } else {
                String providerId = providerAndModelId.substring(0, slashIndex);
                String modelId = providerAndModelId.substring(slashIndex + 1);
                
                agi.getProviders().stream()
                    .filter(p -> p.getProviderId().equals(providerId))
                    .findFirst()
                    .flatMap(provider -> provider.findModel(modelId))
                    .ifPresentOrElse(
                        agi::setSelectedModel,
                        () -> System.out.println("Model not found: " + providerAndModelId)
                    );
            }
        }
        
        // Instantiate the reusable CLI application and run it.
        Cli cliApp = new Cli(agi);
        cliApp.run();
    }
}
