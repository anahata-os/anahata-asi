/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.toolkit.resources.Resources;
import uno.anahata.asi.agi.resource.handle.UrlHandle;
import uno.anahata.asi.toolkit.Audio;
import uno.anahata.asi.toolkit.History;
import uno.anahata.asi.toolkit.Session;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.toolkit.shell.Shell;

/**
 * The definitive configuration blueprint for an individual Agi session.
 * <p>
 * Defines the operational parameters, enabled toolkits, AI providers, 
 * and context management policies. This class serves as the 'DNA' of 
 * an Agi, ensuring architectural consistency and facilitating model-agnostic 
 * orchestration across session restarts.
 * </p>
 * 
 * @author anahata
 */
@Getter
@Setter
public class AgiConfig extends BasicPropertyChangeSource {

    /** A reference to the global, application-wide configuration. */
    @NonNull
    private transient AbstractAsiContainer asiContainer;

    /** The unique identifier for this specific agi session. */
    @NonNull
    private String sessionId;
    
    /** A late-binding reference to the parent Agi. Set during Agi construction. */
    @Setter
    private Agi agi;

    /**
     * The list of AI provider classes available for this agi session.
     */
    private List<Class<? extends AbstractAgiProvider>> providerClasses = new ArrayList<>();
    
    /**
     * The list of tool classes to be used in this agi session.
     */
    private final List<Class<?>> toolClasses = new ArrayList<>();

    {
        // Pre-populate with core, essential tools.
        toolClasses.add(Session.class);
        toolClasses.add(History.class);
        toolClasses.add(Resources.class);
        toolClasses.add(Java.class);        
        toolClasses.add(Shell.class);
        toolClasses.add(Audio.class);
    }

    /**
     * Constructs a new AgiConfig with a randomly generated session ID.
     * 
     * @param asiConfig The global AI configuration.
     */
    public AgiConfig(@NonNull AbstractAsiContainer asiConfig) {
        this(asiConfig, UUID.randomUUID().toString());
    }

    /**
     * Constructs a new AgiConfig with a specific session ID.
     * 
     * @param asiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public AgiConfig(@NonNull AbstractAsiContainer asiConfig, @NonNull String sessionId) {
        this.asiContainer = asiConfig;
        this.sessionId = sessionId;
    }

    //<editor-fold defaultstate="collapsed" desc="Session Loop">
    /** If true, local Java tools are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean localToolsEnabled = true;

    /** If true, server-side tools (like Google Search) are enabled. */
    @Setter(AccessLevel.NONE)
    private boolean hostedToolsEnabled = false;

    /** If true, the agi loop will automatically re-prompt the model after executing tools. */
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
    private int tokenThreshold = 250000; 
    
    /** The default maximum depth a TextPart should be kept in context. */
    private int defaultTextPartMaxDepth = 108;
    
    /** The default maximum depth a ToolResponse should be kept in context. */
    private int defaultToolMaxDepth = 12;
    
    /** The default maximum depth a BlobPart should be kept in context. */
    private int defaultBlobPartMaxDepth = 4;

    /** The default maximum depth a model thought should be kept in context. */
    private int defaultThoughtPartMaxDepth = 12;
    //</editor-fold>

    /** The default response modalities for this agi session. */
    private List<String> defaultResponseModalities = new ArrayList<>(List.of("TEXT"));
    
    /**
     * Configures the session to utilize local Java-based toolkits.
     * <p>
     * Enabling local tools automatically disables hosted server tools to 
     * maintain environmental consistency.
     * </p>
     * 
     * @param enabled true to enable local toolkits.
     */
    public void setLocalToolsEnabled(boolean enabled) {
        boolean oldServer = this.hostedToolsEnabled;
        this.localToolsEnabled = enabled;
        if (enabled) {
            this.hostedToolsEnabled = false;
        }
        propertyChangeSupport.firePropertyChange("hostedToolsEnabled", oldServer, this.hostedToolsEnabled);
    }

    /**
     * Configures the session to utilize hosted server-side tools (e.g., Google Search).
     * <p>
     * Enabling hosted tools automatically disables local Java toolkits to 
     * prevent capability conflicts.
     * </p>
     * 
     * @param enabled true to enable server-side tools.
     */
    public void setHostedToolsEnabled(boolean enabled) {
        boolean oldServer = this.hostedToolsEnabled;
        this.hostedToolsEnabled = enabled;
        if (enabled) {
            this.localToolsEnabled = false;
        }
        propertyChangeSupport.firePropertyChange("hostedToolsEnabled", oldServer, enabled);
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
     * Creates a specialized {@link ResourceHandle} based on the URI scheme.
     * <p>
     * Supports 'file' schemes via {@link PathHandle} and fallbacks to 
     * {@link UrlHandle} for remote resources.
     * </p>
     * 
     * @param uri The resource URI to wrap.
     * @return A concrete handle implementation for the specified URI.
     */
    public ResourceHandle createResourceHandle(java.net.URI uri) {
        if ("file".equalsIgnoreCase(uri.getScheme())) {
            return new PathHandle(uri.getPath());
        }
        return new UrlHandle(uri.toString());
    }
    
    /**
     * Convenience method to get the host application ID from the parent AsiContainer.
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return asiContainer.getHostApplicationId();
    }
}
