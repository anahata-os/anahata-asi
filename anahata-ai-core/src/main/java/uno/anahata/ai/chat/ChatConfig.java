/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.chat;

import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.tool.files.Files;

/**
 * A model-agnostic, intelligent configuration object for a single chat session.
 * It defines the blueprint for a chat, including which AI providers and tools are available.
 */
@Getter
@Setter
@RequiredArgsConstructor
public class ChatConfig {

    /** A reference to the global, application-wide configuration. */
    @NonNull
    private final AiConfig aiConfig;

    /** The unique identifier for this specific chat session. */
    @NonNull
    private final String sessionId;
    
    /** The user-defined name for this chat session. Defaults to the session ID. */
    private String name;
    
    /** A late-binding reference to the parent Chat. Set during Chat construction. */
    @Setter
    private Chat chat;

    /**
     * The list of AI provider classes available for this chat session.
     * The Chat orchestrator will use this list to discover and instantiate providers.
     */
    private List<Class<? extends AbstractAiProvider>> providerClasses = new ArrayList<>();
    
    /**
     * The list of tool classes to be used in this chat session.
     * This is pre-populated with core tools and can be modified by the user
     * before the Chat session is created.
     */
    private final List<Class<?>> toolClasses = new ArrayList<>();

    {
        // Pre-populate with core, essential tools.
        toolClasses.add(Files.class);
    }

    /** The default request configuration for this chat session. Lazily initialized. */
    private RequestConfig requestConfig;
    
    //<editor-fold defaultstate="collapsed" desc="Chat Loop">
    /** If true, local Java tools are enabled. If false, server-side tools (like Google Search) are used. */
    private boolean localToolsEnabled = true;

    /** If true, the chat loop will automatically re-prompt the model after executing tools. */
    private boolean autoReplyTools = false;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Context Management">
    /** The default number of user turns a TextPart should be kept in context. */
    private int defaultTextPartTurnsToKeep = 108;
    
    /** The default number of user turns a ToolResponse should be kept in context. */
    private int defaultToolTurnsToKeep = 5;
    
    /** The default number of user turns a BlobPart should be kept in context. */
    private int defaultBlobPartTurnsToKeep = 3;
    
    /** The number of turns a part must be soft-pruned before it is eligible for hard-pruning (permanent deletion). */
    private int hardPruneDelay = 108;
    //</editor-fold>
    
    /**
     * Gets the request configuration, initializing it with a reference to the chat
     * on first access.
     * @return The request configuration.
     */
    public RequestConfig getRequestConfig() {
        if (requestConfig == null) {
            if (chat == null) {
                throw new IllegalStateException("Chat reference has not been set in ChatConfig");
            }
            requestConfig = new RequestConfig(chat);
        }
        return requestConfig;
    }
    
    /**
     * Convenience method to get the host application ID from the parent AiConfig.
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return aiConfig.getHostApplicationId();
    }
}
