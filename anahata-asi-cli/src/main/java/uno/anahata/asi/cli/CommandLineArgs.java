/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.cli;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;

/**
 * A centralized utility for parsing command-line arguments for any Anahata AI launcher.
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class CommandLineArgs {

    /**
     * Parses the command-line arguments to find and select a specific AI model.
     *
     * @param agi The Agi instance to configure.
     * @param args The command-line arguments from the main method.
     */
    public static void parse(Agi agi, String[] args) {
        if (args == null || args.length == 0) {
            return;
        }

        String providerAndModelId = args[0];
        log.info("Attempting to select model from argument: {}", providerAndModelId);

        int slashIndex = providerAndModelId.indexOf('/');

        if (slashIndex <= 0 || slashIndex == providerAndModelId.length() - 1) {
            log.warn("Invalid model format. Expected 'providerId/modelId'. Using default model.");
            return;
        }

        String providerId = providerAndModelId.substring(0, slashIndex);
        String modelId = providerAndModelId.substring(slashIndex + 1);
        
        log.info("Attempting to select model from arguments: {} {}, agi providers: ", providerId, modelId, agi.getProviders());

        agi.getProviders().stream()
            .filter(p -> p.getProviderId().equals(providerId))
            .findFirst()
            .flatMap(provider -> provider.findModel(modelId))
            .ifPresentOrElse(
                agi::setSelectedModel,
                () -> log.error("Model not found from command-line argument: {}", providerAndModelId)
            );
    }
}
