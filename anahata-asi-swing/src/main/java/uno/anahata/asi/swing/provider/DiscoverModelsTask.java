/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.provider;

import java.awt.Component;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized background task for discovering AI models from provider remote APIs.
 * <p>
 * This task queries the provider's live endpoint into {@code cachedApiModels}. When running in
 * interactive mode (e.g. triggered by 'Test Connection' or 'Refresh'), it evaluates newly discovered
 * models that are not yet persisted in the local database and prompts the user with an interactive
 * confirmation dialog to add and persist them to disk.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class DiscoverModelsTask extends SwingTask<List<AbstractModel>> {

    /**
     * The target AI provider whose endpoint is being queried.
     */
    private final AbstractAiProvider provider;

    /**
     * Whether this task was initiated interactively by user action.
     */
    private final boolean interactive;

    /**
     * Optional callback invoked on the Event Dispatch Thread upon task completion.
     */
    private final Consumer<List<AbstractModel>> onCompleteCallback;

    /**
     * Constructs a DiscoverModelsTask with full interactive controls and parent component anchoring.
     *
     * @param owner The parent UI component for positioning confirmation and alert dialogs.
     * @param provider The target AI provider instance.
     * @param interactive Whether to display modal notifications and confirmation prompts.
     * @param onCompleteCallback Optional callback invoked with the list of newly discovered models.
     */
    public DiscoverModelsTask(Component owner, AbstractAiProvider provider, boolean interactive, Consumer<List<AbstractModel>> onCompleteCallback) {
        super(owner, Objects.requireNonNull(provider, "provider cannot be null").getAsiContainer(),
                "Discovering Models: " + provider.getDisplayName(),
                () -> (List<AbstractModel>) provider.refreshCachedApiModels());
        this.provider = provider;
        this.interactive = interactive;
        this.onCompleteCallback = onCompleteCallback;

        setOnDone(apiModels -> handleDiscoveryCompleted(apiModels));
        setOnError(error -> handleDiscoveryError(error));
    }

    /**
     * Constructs a DiscoverModelsTask with a completion callback.
     *
     * @param provider The target AI provider instance.
     * @param interactive Whether to display modal notifications and confirmation prompts.
     * @param onCompleteCallback Optional callback invoked with the list of newly discovered models.
     */
    public DiscoverModelsTask(AbstractAiProvider provider, boolean interactive, Consumer<List<AbstractModel>> onCompleteCallback) {
        this(null, provider, interactive, onCompleteCallback);
    }

    /**
     * Constructs a DiscoverModelsTask without custom completion callbacks.
     *
     * @param provider The target AI provider instance.
     * @param interactive Whether to display modal notifications and confirmation prompts.
     */
    public DiscoverModelsTask(AbstractAiProvider provider, boolean interactive) {
        this(null, provider, interactive, null);
    }

    /**
     * Processes discovery completion on the Event Dispatch Thread (EDT).
     *
     * @param apiModels The list of models returned from the remote API.
     */
    private void handleDiscoveryCompleted(List<AbstractModel> apiModels) {
        Set<String> localIds = provider.getModels().stream()
                .map(AbstractModel::getModelId)
                .collect(Collectors.toSet());
        List<AbstractModel> newModels = provider.getCachedApiModels().stream()
                .filter(m -> !localIds.contains(m.getModelId()))
                .collect(Collectors.toList());

        log.info("Discovered {} total model(s) from API for '{}' ({} new/unregistered)",
                apiModels != null ? apiModels.size() : 0, provider.getDisplayName(), newModels.size());

        if (provider.isAutomaticallyRegisterNewlyDiscoveredModels() && !newModels.isEmpty()) {
            try {
                provider.addModels(newModels);
                log.info("Auto-registered and persisted {} new model(s) for '{}'", newModels.size(), provider.getDisplayName());
            } catch (IOException ex) {
                log.error("Failed to auto-persist discovered models for '{}'", provider.getDisplayName(), ex);
            }
            if (interactive) {
                JOptionPane.showMessageDialog(getOwner(),
                        "Connection OK!\n\nDiscovered and automatically added " + newModels.size() + " new model(s) to '" + provider.getDisplayName() + "'.",
                        "Models Synchronized", JOptionPane.INFORMATION_MESSAGE);
            }
        } else if (interactive) {
            if (newModels.isEmpty()) {
                JOptionPane.showMessageDialog(getOwner(),
                        "Connection OK!\n\nProvider '" + provider.getDisplayName() + "' is reachable.\n"
                        + "All " + (apiModels != null ? apiModels.size() : 0) + " remote model(s) are already synchronized.",
                        "Connection Verified", JOptionPane.INFORMATION_MESSAGE);
            } else {
                int option = JOptionPane.showConfirmDialog(getOwner(),
                        "Connection OK!\n\nDiscovered " + newModels.size() + " new model(s) for '" + provider.getDisplayName() + "'.\n\n"
                        + "Would you like to add and persist them to your local database now?",
                        "New Models Discovered", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                if (option == JOptionPane.YES_OPTION) {
                    try {
                        provider.addModels(newModels);
                    } catch (IOException ex) {
                        log.error("Failed to persist discovered models for '{}'", provider.getDisplayName(), ex);
                    }
                    JOptionPane.showMessageDialog(getOwner(),
                            "Successfully added and persisted " + newModels.size() + " new model(s) to '" + provider.getDisplayName() + "'.",
                            "Models Added", JOptionPane.INFORMATION_MESSAGE);
                }
            }
        }

        if (onCompleteCallback != null) {
            onCompleteCallback.accept(newModels);
        }
    }

    /**
     * Handles discovery errors on the Event Dispatch Thread (EDT).
     *
     * @param error The exception that occurred during API communication.
     */
    private void handleDiscoveryError(Exception error) {
        log.error("Model discovery failed for provider '{}'", provider.getDisplayName(), error);
        if (interactive) {
            JOptionPane.showMessageDialog(getOwner(),
                    "Connection / Model discovery failed for '" + provider.getDisplayName() + "':\n\n"
                    + error.getMessage(),
                    "Discovery Failed", JOptionPane.ERROR_MESSAGE);
        }
    }
}
