package uno.anahata.ai.config;

import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.provider.AbstractAiProvider;

/**
 * A model-agnostic, intelligent configuration object for a single chat session.
 * It defines the blueprint for a chat, including which AI providers and tools are available.
 */
@Getter
@RequiredArgsConstructor
public class ChatConfig {

    /** A reference to the global, application-wide configuration. */
    @NonNull
    private final AiConfig aiConfig;

    /** The unique identifier for this specific chat session. */
    @NonNull
    private final String sessionId;

    /**
     * The list of AI provider classes available for this chat session.
     * The Chat orchestrator will use this list to discover and instantiate providers.
     */
    @Getter
    protected List<Class<? extends AbstractAiProvider>> providerClasses = new ArrayList<>();
    
    /**
     * The list of tool classes to be used in this chat session.
     * This can be overridden by subclasses to provide a custom set of tools.
     */
    @Getter
    protected List<Class<?>> toolClasses = new ArrayList<>();

    /**
     * Convenience method to get the host application ID from the parent AiConfig.
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return aiConfig.getHostApplicationId();
    }
}
