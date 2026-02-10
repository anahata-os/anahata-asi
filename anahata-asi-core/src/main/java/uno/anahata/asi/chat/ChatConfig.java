/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.provider.AbstractAiProvider;
import uno.anahata.asi.toolkit.Session;
import uno.anahata.asi.toolkit.files.Files;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.toolkit.Resources;
import uno.anahata.asi.toolkit.shell.Shell;

/**
 * A model-agnostic, intelligent configuration object for a single chat session.
 * It defines the blueprint for a chat, including which AI providers and tools are available,
 * as well as the default context management policies.
 * 
 * @author anahata
 */
@Getter
@Setter
public class ChatConfig extends BasicPropertyChangeSource {

    /** A reference to the global, application-wide configuration. */
    @NonNull
    private transient AsiContainer container;

    /** The unique identifier for this specific chat session. */
    @NonNull
    private String sessionId;
    
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
        toolClasses.add(Session.class);
        toolClasses.add(Files.class);
        toolClasses.add(Java.class);        
        toolClasses.add(Resources.class);
        toolClasses.add(Shell.class);
    }

    /**
     * Constructs a new ChatConfig with a randomly generated session ID.
     * 
     * @param asiConfig The global AI configuration.
     */
    public ChatConfig(@NonNull AsiContainer asiConfig) {
        this(asiConfig, UUID.randomUUID().toString());
    }

    /**
     * Constructs a new ChatConfig with a specific session ID.
     * 
     * @param asiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public ChatConfig(@NonNull AsiContainer asiConfig, @NonNull String sessionId) {
        this.container = asiConfig;
        this.sessionId = sessionId;
    }

    /**
     * Re-binds this configuration to an AsiContainer after deserialization.
     * 
     * @param container The AsiContainer to bind to.
     */
    public void rebind(@NonNull AsiContainer container) {
        this.container = container;
    }

    //<editor-fold defaultstate="collapsed" desc="Session Loop">
    /** If true, local Java tools are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean localToolsEnabled = true;

    /** If true, server-side tools (like Google Search) are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean serverToolsEnabled = false;

    /** If true, the chat loop will automatically re-prompt the model after executing tools. */
    private boolean autoReplyTools = true;

    /** If true, token streaming is enabled for model responses. */
    private boolean streaming = true;
    
    /** If true, the model is requested to include its internal thought process in the response. */
    private boolean includeThoughts = true;

    /** If true, thought parts are initially expanded in the UI. */
    private boolean expandThoughts = false;

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
    
    /** The default maximum depth a TextPart should be kept in context. */
    private int defaultTextPartMaxDepth = 108;
    
    /** The default maximum depth a ToolResponse should be kept in context. */
    private int defaultToolMaxDepth = 12;
    
    /** The default maximum depth a BlobPart should be kept in context. */
    private int defaultBlobPartMaxDepth = 3;
    //</editor-fold>

    /** The default response modalities for this chat session. */
    private List<String> defaultResponseModalities = new ArrayList<>(List.of("TEXT"));
    
    /**
     * Sets whether local tools are enabled. Enabling local tools automatically
     * disables server-side tools.
     * 
     * @param enabled true to enable local tools.
     */
    public void setLocalToolsEnabled(boolean enabled) {
        boolean oldServer = this.serverToolsEnabled;
        this.localToolsEnabled = enabled;
        if (enabled) {
            this.serverToolsEnabled = false;
        }
        // Only fire serverToolsEnabled to trigger a single UI sync
        propertyChangeSupport.firePropertyChange("serverToolsEnabled", oldServer, this.serverToolsEnabled);
    }

    /**
     * Sets whether server-side tools are enabled. Enabling server-side tools
     * automatically disables local tools.
     * 
     * @param enabled true to enable server-side tools.
     */
    public void setServerToolsEnabled(boolean enabled) {
        boolean oldServer = this.serverToolsEnabled;
        this.serverToolsEnabled = enabled;
        if (enabled) {
            this.localToolsEnabled = false;
        }
        propertyChangeSupport.firePropertyChange("serverToolsEnabled", oldServer, enabled);
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
     * Convenience method to get the host application ID from the parent AsiContainer.
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return container.getHostApplicationId();
    }
}
