/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.chat;

import java.util.ArrayList;
import java.util.List;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.toolkit.Files;
import uno.anahata.ai.toolkit.Java;

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
    private String sessionId;
    
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
        toolClasses.add(Java.class);
    }

    /** The default request configuration for this chat session. Lazily initialized. */
    private RequestConfig requestConfig;
    
    //<editor-fold defaultstate="collapsed" desc="Chat Loop">
    /** If true, local Java tools are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean localToolsEnabled = true;

    /** If true, server-side tools (like Google Search) are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean serverToolsEnabled = false;

    /** If true, the chat loop will automatically re-prompt the model after executing tools. */
    private boolean autoReplyTools = false;

    /** If true, token streaming is enabled for model responses. */
    private boolean streaming = true;
    
    /** The maximum number of times to retry an API call on failure. */
    private int apiMaxRetries = 5;

    /** The initial delay in milliseconds before the first retry. */
    private long apiInitialDelayMillis = 2000;

    /** The maximum delay in milliseconds between retries. */
    private long apiMaxDelayMillis = 30000;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Context Management">
    /** The maximum number of tokens allowed in the context window. */
    private int tokenThreshold = 250000; // Moved from ContextManager
    
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
     * Sets whether local tools are enabled. Enabling local tools automatically
     * disables server-side tools.
     * 
     * @param enabled true to enable local tools.
     */
    public void setLocalToolsEnabled(boolean enabled) {
        this.localToolsEnabled = enabled;
        if (enabled) {
            this.serverToolsEnabled = false;
        }
    }

    /**
     * Sets whether server-side tools are enabled. Enabling server-side tools
     * automatically disables local tools.
     * 
     * @param enabled true to enable server-side tools.
     */
    public void setServerToolsEnabled(boolean enabled) {
        this.serverToolsEnabled = enabled;
        if (enabled) {
            this.localToolsEnabled = false;
        }
    }

    /**
     * Gets the maximum number of tokens allowed in the context window.
     *
     * @return The token threshold.
     */
    public int getTokenThreshold() {
        return tokenThreshold;
    }
    
    /**
     * Convenience method to get the host application ID from the parent AiConfig.
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return aiConfig.getHostApplicationId();
    }
}
