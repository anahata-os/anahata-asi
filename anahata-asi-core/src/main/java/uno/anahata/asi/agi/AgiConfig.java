/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.toolkit.resources.Resources;
import uno.anahata.asi.agi.resource.handle.UrlHandle;
import uno.anahata.asi.toolkit.AsiContainer;
import uno.anahata.asi.toolkit.Audio;
import uno.anahata.asi.toolkit.History;
import uno.anahata.asi.toolkit.Session;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.toolkit.shell.Shell;

/**
 * The definitive configuration blueprint for an individual Agi session.
 * <p>
 * Defines the operational parameters, enabled toolkits, AI providers, and
 * context management policies. This class serves as the 'DNA' of an Agi,
 * ensuring architectural consistency and facilitating model-agnostic
 * orchestration across session restarts.
 * </p>
 *
 * @author anahata
 */
@Getter
@Setter
public class AgiConfig extends BasicPropertyChangeSource {

    /**
     * A reference to the global, application-wide configuration.
     */
    @NonNull
    private transient AbstractAsiContainer asiContainer;

    /**
     * The unique identifier for this specific agi session.
     */
    @NonNull
    private String sessionId;

    /**
     * The UUID of the AGI that spawned this session, if any.
     */
    private String parentUuid;

    /**
     * A late-binding reference to the parent Agi. Set during Agi construction.
     */
    @Setter
    private Agi agi;

    /**
     * The UUID of the AI provider currently selected for this session.
     */
    private String selectedProviderUuid;

    /**
     * The ID of the AI model currently selected for this session.
     */
    private String selectedModelId;

    /**
     * The list of AI provider UUIDs available for this agi session.
     */
    private List<String> providerUuids = new ArrayList<>();

    /**
     * The list of tool classes to be used in this agi session.
     */
    private final List<Class<?>> toolClasses = new ArrayList<>();

    {
        // Pre-populate with core, essential tools.
        toolClasses.add(AsiContainer.class);
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

        // Populate available providers from the container's registry
        for (AbstractAiProvider provider : asiConfig.getAllProviders()) {
            if (provider.isEnabled()) {
                providerUuids.add(provider.getUuid());
            }
        }
    }

    //<editor-fold defaultstate="collapsed" desc="Session Loop">
    /**
     * If true, local Java tools are enabled.
     */
    @Setter(AccessLevel.NONE)
    private boolean localToolsEnabled = true;

    /**
     * If true, server-side tools (like Google Search) are enabled.
     */
    @Setter(AccessLevel.NONE)
    private boolean hostedToolsEnabled = false;

    /**
     * If true, the agi loop will automatically re-prompt the model after
     * executing tools.
     */
    private boolean autoReplyTools = true;

    /**
     * If true, token streaming is enabled for model responses.
     */
    private boolean streaming = true;

    /**
     * If true, the model is requested to include its internal thought process
     * in the response.
     */
    private boolean includeThoughts = true;

    /**
     * If true, thought parts are initially expanded in the UI.
     */
    private boolean expandThoughts = false;
    /**
     * Preference for expanding tool calls in the UI.
     */
    private ExpandToolsPreference expandTools = ExpandToolsPreference.NONE;

    /**
     * The maximum number of times to retry an API call on failure.
     */
    private int apiMaxRetries = 5;

    /**
     * The initial delay in milliseconds before the first retry.
     */
    private long apiInitialDelayMillis = 2000;

    /**
     * The maximum delay in milliseconds between retries.
     */
    private long apiMaxDelayMillis = 30000;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Context Management">
    /**
     * The maximum number of tokens allowed in the context window.
     */
    private int tokenThreshold = 250000;

    /**
     * The default maximum depth a BlobPart should be kept in context.
     */
    private int defaultBlobPartMaxDepth = 4;

    /**
     * The default maximum depth a ToolResponse should be kept in context.
     */
    private int defaultToolMaxDepth = 8;

    /**
     * The default maximum depth a model thought should be kept in context.
     */
    private int defaultThoughtPartMaxDepth = 6;

    /**
     * The default maximum depth a TextPart should be kept in context.
     */
    private int defaultTextPartMaxDepth = 108;

    /**
     * The default maximum depth a model code execution part should be kept in context.
     */
    private int defaultModelCodeExecutionMaxDepth = 8;

    /**
     * The default maximum depth a web search call part should be kept in context.
     */
    private int defaultWebSearchMaxDepth = 8;

    //</editor-fold>
    /**
     * The default response modalities for this agi session.
     */
    private List<String> defaultResponseModalities = new ArrayList<>(List.of("TEXT"));

    /**
     * Sets the selected provider UUID and fires a property change event.
     * @param selectedProviderUuid The UUID of the selected provider.
     */
    public void setSelectedProviderUuid(String selectedProviderUuid) {
        String old = this.selectedProviderUuid;
        if (!Objects.equals(old, selectedProviderUuid)) {
            this.selectedProviderUuid = selectedProviderUuid;
            propertyChangeSupport.firePropertyChange("selectedProviderUuid", old, selectedProviderUuid);
        }
    }

    /**
     * Sets the selected model ID and fires a property change event.
     * @param selectedModelId The ID of the selected model.
     */
    public void setSelectedModelId(String selectedModelId) {
        String old = this.selectedModelId;
        if (!Objects.equals(old, selectedModelId)) {
            this.selectedModelId = selectedModelId;
            propertyChangeSupport.firePropertyChange("selectedModelId", old, selectedModelId);
        }
    }

    /**
     * Sets whether tool calls should be auto-replied and fires a property change event.
     * @param autoReplyTools true to enable auto-reply.
     */
    public void setAutoReplyTools(boolean autoReplyTools) {
        boolean old = this.autoReplyTools;
        if (old != autoReplyTools) {
            this.autoReplyTools = autoReplyTools;
            propertyChangeSupport.firePropertyChange("autoReplyTools", old, autoReplyTools);
        }
    }

    /**
     * Sets whether streaming is enabled and fires a property change event.
     * @param streaming true to enable streaming.
     */
    public void setStreaming(boolean streaming) {
        boolean old = this.streaming;
        if (old != streaming) {
            this.streaming = streaming;
            propertyChangeSupport.firePropertyChange("streaming", old, streaming);
        }
    }

    /**
     * Sets whether thoughts should be included and fires a property change event.
     * @param includeThoughts true to include thoughts.
     */
    public void setIncludeThoughts(boolean includeThoughts) {
        boolean old = this.includeThoughts;
        if (old != includeThoughts) {
            this.includeThoughts = includeThoughts;
            propertyChangeSupport.firePropertyChange("includeThoughts", old, includeThoughts);
        }
    }

    /**
     * Sets the tool expansion preference and fires a property change event.
     * @param expandTools The expansion preference.
     */
    public void setExpandTools(ExpandToolsPreference expandTools) {
        ExpandToolsPreference old = this.expandTools;
        if (old != expandTools) {
            this.expandTools = expandTools;
            propertyChangeSupport.firePropertyChange("expandTools", old, expandTools);
        }
    }

    /**
     * Sets whether thoughts should be expanded and fires a property change event.
     * @param expandThoughts true to expand thoughts.
     */
    public void setExpandThoughts(boolean expandThoughts) {
        boolean old = this.expandThoughts;
        if (old != expandThoughts) {
            this.expandThoughts = expandThoughts;
            propertyChangeSupport.firePropertyChange("expandThoughts", old, expandThoughts);
        }
    }

    /**
     * Sets the max API retries and fires a property change event.
     * @param apiMaxRetries The maximum number of retries.
     */
    public void setApiMaxRetries(int apiMaxRetries) {
        int old = this.apiMaxRetries;
        if (old != apiMaxRetries) {
            this.apiMaxRetries = apiMaxRetries;
            propertyChangeSupport.firePropertyChange("apiMaxRetries", old, apiMaxRetries);
        }
    }

    /**
     * Sets the initial API retry delay and fires a property change event.
     * @param apiInitialDelayMillis The initial delay in milliseconds.
     */
    public void setApiInitialDelayMillis(long apiInitialDelayMillis) {
        long old = this.apiInitialDelayMillis;
        if (old != apiInitialDelayMillis) {
            this.apiInitialDelayMillis = apiInitialDelayMillis;
            propertyChangeSupport.firePropertyChange("apiInitialDelayMillis", old, apiInitialDelayMillis);
        }
    }

    /**
     * Sets the max API retry delay and fires a property change event.
     * @param apiMaxDelayMillis The maximum delay in milliseconds.
     */
    public void setApiMaxDelayMillis(long apiMaxDelayMillis) {
        long old = this.apiMaxDelayMillis;
        if (old != apiMaxDelayMillis) {
            this.apiMaxDelayMillis = apiMaxDelayMillis;
            propertyChangeSupport.firePropertyChange("apiMaxDelayMillis", old, apiMaxDelayMillis);
        }
    }

    /**
     * Sets the token threshold and fires a property change event.
     * @param tokenThreshold The threshold token count.
     */
    public void setTokenThreshold(int tokenThreshold) {
        int old = this.tokenThreshold;
        if (old != tokenThreshold) {
            this.tokenThreshold = tokenThreshold;
            propertyChangeSupport.firePropertyChange("tokenThreshold", old, tokenThreshold);
        }
    }

    /**
     * Sets the default text part max depth and fires a property change event.
     * @param defaultTextPartMaxDepth The default max depth.
     */
    public void setDefaultTextPartMaxDepth(int defaultTextPartMaxDepth) {
        int old = this.defaultTextPartMaxDepth;
        if (old != defaultTextPartMaxDepth) {
            this.defaultTextPartMaxDepth = defaultTextPartMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultTextPartMaxDepth", old, defaultTextPartMaxDepth);
        }
    }

    /**
     * Sets the default tool response max depth and fires a property change event.
     * @param defaultToolMaxDepth The default max depth.
     */
    public void setDefaultToolMaxDepth(int defaultToolMaxDepth) {
        int old = this.defaultToolMaxDepth;
        if (old != defaultToolMaxDepth) {
            this.defaultToolMaxDepth = defaultToolMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultToolMaxDepth", old, defaultToolMaxDepth);
        }
    }

    /**
     * Sets the default blob part max depth and fires a property change event.
     * @param defaultBlobPartMaxDepth The default max depth.
     */
    public void setDefaultBlobPartMaxDepth(int defaultBlobPartMaxDepth) {
        int old = this.defaultBlobPartMaxDepth;
        if (old != defaultBlobPartMaxDepth) {
            this.defaultBlobPartMaxDepth = defaultBlobPartMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultBlobPartMaxDepth", old, defaultBlobPartMaxDepth);
        }
    }

    /**
     * Sets the default thought part max depth and fires a property change event.
     * @param defaultThoughtPartMaxDepth The default max depth.
     */
    public void setDefaultThoughtPartMaxDepth(int defaultThoughtPartMaxDepth) {
        int old = this.defaultThoughtPartMaxDepth;
        if (old != defaultThoughtPartMaxDepth) {
            this.defaultThoughtPartMaxDepth = defaultThoughtPartMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultThoughtPartMaxDepth", old, defaultThoughtPartMaxDepth);
        }
    }

    /**
     * Sets the default model code execution max depth and fires a property change event.
     * @param defaultModelCodeExecutionMaxDepth The default max depth.
     */
    public void setDefaultModelCodeExecutionMaxDepth(int defaultModelCodeExecutionMaxDepth) {
        int old = this.defaultModelCodeExecutionMaxDepth;
        if (old != defaultModelCodeExecutionMaxDepth) {
            this.defaultModelCodeExecutionMaxDepth = defaultModelCodeExecutionMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultModelCodeExecutionMaxDepth", old, defaultModelCodeExecutionMaxDepth);
        }
    }

    /**
     * Sets the default web search max depth and fires a property change event.
     * @param defaultWebSearchMaxDepth The default max depth.
     */
    public void setDefaultWebSearchMaxDepth(int defaultWebSearchMaxDepth) {
        int old = this.defaultWebSearchMaxDepth;
        if (old != defaultWebSearchMaxDepth) {
            this.defaultWebSearchMaxDepth = defaultWebSearchMaxDepth;
            propertyChangeSupport.firePropertyChange("defaultWebSearchMaxDepth", old, defaultWebSearchMaxDepth);
        }
    }

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
     * Configures the session to utilize hosted server-side tools (e.g., Google
     * Search).
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
     * Convenience method to get the host application ID from the parent
     * AsiContainer.
     *
     * @return The host application ID.
     */
    public String getHostApplicationId() {
        return asiContainer.getHostApplicationId();
    }
}
