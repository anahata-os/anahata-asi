/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/anahata-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.asi.tool;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiConfig;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.bad.BadTool;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.model.tool.java.JavaObjectToolkit;

/**
 * Manages the lifecycle of all AI tools, including registration, configuration,
 * and lookup. It stores toolkits and generates the full list of tools on
 * demand.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
public class ToolManager extends BasicPropertyChangeSource {

    /** A static counter for generating unique, sequential tool call IDs. */
    private static final AtomicInteger callIdGenerator = new AtomicInteger(0);

    /** The parent chat session. Can be null in test environments. */
    private final Chat chat;
    
    /** The global AI configuration. */
    private final AsiConfig config;
    
    /** A map of registered toolkits, keyed by their simple name. */
    private final Map<String, AbstractToolkit<?>> toolkits = new HashMap<>();
    
    /** A thread-safe list of currently executing tool calls. */
    private final List<AbstractToolCall<?, ?>> executingCalls = new CopyOnWriteArrayList<>();

    /**
     * Primary constructor for use in a live chat session.
     * This constructor is now self-initializing, registering all tools
     * defined in the ChatConfig.
     *
     * @param chat The parent chat orchestrator.
     */
    public ToolManager(@NonNull Chat chat) {
        this.chat = chat;
        this.config = chat.getConfig().getAsiConfig();
        
        // Self-initialize by registering tools from the config.
        List<Class<?>> toolClasses = chat.getConfig().getToolClasses();
        if (toolClasses != null) {
            registerClasses(toolClasses.toArray(new Class[0]));
        }
    }

    /**
     * Secondary constructor for lightweight instantiation, primarily for unit
     * tests.
     *
     * @param config The global AI configuration.
     */
    public ToolManager(@NonNull AsiConfig config) {
        this.chat = null; // No parent chat in this context
        this.config = config;
    }
    
    /**
     * Resets the static tool call ID counter to zero.
     */
    public void reset() {
        callIdGenerator.set(0);
        log.info("ToolManager call ID counter has been reset.");
    }

    /**
     * Scans the given classes for methods annotated with {@link AiTool},
     * creates the corresponding toolkits, and applies any application-wide
     * preferences.
     *
     * @param classes The classes to scan for tools.
     */
    public final void registerClasses(Class<?>... classes) {
        log.info("Registering tool classes...");
        for (Class<?> clazz : classes) {
            try {
                JavaObjectToolkit toolkit = new JavaObjectToolkit(this, clazz);
                toolkits.put(toolkit.getName(), toolkit);
                log.info("Registered toolkit: {}", toolkit.getName());
            } catch (Exception e) {
                log.error("Failed to register toolkit for class: {}", clazz.getName(), e);
            }
        }
        applyPreferences();
    }

    /**
     * The primary factory method for creating a model-agnostic tool call from
     * provider-specific data. This method orchestrates the creation and
     * pre-rejection logic.
     *
     * @param amm The model message that initiated the call.
     * @param id The unique ID of the tool call (can be null).
     * @param name The name of the tool to call.
     * @param jsonArgs The raw arguments from the model.
     * @return An {@link AbstractToolCall} with its corresponding, possibly
     * pre-rejected, response.
     */
    public AbstractToolCall createToolCall(AbstractModelMessage amm, String id, String name, Map<String, Object> jsonArgs) {
        String callId = (id == null || id.isEmpty()) ? String.valueOf(callIdGenerator.incrementAndGet()) : id;

        Optional<? extends AbstractTool> toolOpt = findToolByName(name);
        log.info("Found tool: " + toolOpt);
        AbstractTool tool;
        if (toolOpt.isPresent()) {
            tool = toolOpt.get();
        } else {
            tool = new BadTool(name);
        }

        AbstractToolCall call = tool.createCall(amm, callId, jsonArgs);

        // Post-creation checks
        AbstractToolkit toolkit = tool.getToolkit();
        if (toolkit != null && !toolkit.isEnabled()) {
            String reason = "Tool call rejected: The toolkit '" + toolkit.getName() + "' is disabled.";
            log.warn(reason);
            call.getResponse().reject(reason);
        }

        if (tool.getPermission() == ToolPermission.DENY_NEVER) {
            String reason = "Tool call rejected: The tool '" + name + "' has a DENY_NEVER permission.";
            log.warn(reason);
            call.getResponse().reject(reason);
        }

        return call;
    }

    /**
     * Finds a tool by its fully qualified name.
     * 
     * @param name The tool name.
     * @return An Optional containing the tool if found.
     */
    private Optional<? extends AbstractTool> findToolByName(String name) {
        return getAllTools().stream()
                .filter(t -> t.getName().equals(name))
                .findFirst();
    }

    /**
     * Gets a list of all toolkits that are currently enabled.
     * 
     * @return The list of enabled toolkits.
     */
    public List<AbstractToolkit<?>> getEnabledToolkits() {
        return toolkits.values().stream()
                .filter(AbstractToolkit::isEnabled)
                .collect(Collectors.toList());
    }

    /**
     * Gets a list of all toolkits that are currently disabled.
     * 
     * @return The list of disabled toolkits.
     */
    public List<AbstractToolkit<?>> getDisabledToolkits() {
        return toolkits.values().stream()
                .filter(tk -> !tk.isEnabled())
                .collect(Collectors.toList());
    }

    /**
     * Gets a dynamically aggregated list of all tools from all registered
     * toolkits. This is a view and is generated on each call.
     *
     * @return A list of all tools.
     */
    public List<? extends AbstractTool> getAllTools() {
        return toolkits.values().stream()
                .map(AbstractToolkit::getAllTools)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
    }

    /**
     * Gets a list of all tools that are currently enabled and allowed to be
     * sent to the model.
     *
     * @return A filtered list of enabled tools.
     */
    public List<? extends AbstractTool> getEnabledTools() {
        return getEnabledToolkits().stream()
                .map(AbstractToolkit::getAllowedTools)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
    }

    /**
     * Applies application-wide tool preferences to all registered tools.
     */
    private void applyPreferences() {
        log.info("Applying application-wide tool preferences...");
        // TODO: Implement logic to apply preferences from config.getPreferences()
        // to each tool in getAllTools().
    }
    
    /**
     * Returns all 'enabled' toolkits that implement ContextProvider.
     * 
     * @return enabled toolkits that implement ContextProvider.
     */
    public List<ContextProvider> getContextProviderTools() {
        List<ContextProvider> ret = new ArrayList<>();
        for (AbstractToolkit at: getEnabledToolkits()) {
            if (at instanceof ContextProvider cp) {
                ret.add(cp);
            }
        }
        return ret;
    }
    
    /**
     * Registers a tool call as currently executing and fires a property change event.
     * 
     * @param call The tool call to register.
     */
    public void registerExecutingCall(AbstractToolCall<?, ?> call) {
        List<AbstractToolCall<?, ?>> oldCalls = new ArrayList<>(executingCalls);
        executingCalls.add(call);
        getPropertyChangeSupport().firePropertyChange("executingCalls", oldCalls, executingCalls);
    }

    /**
     * Unregisters a tool call from the executing list and fires a property change event.
     * 
     * @param call The tool call to unregister.
     */
    public void unregisterExecutingCall(AbstractToolCall<?, ?> call) {
        List<AbstractToolCall<?, ?>> oldCalls = new ArrayList<>(executingCalls);
        executingCalls.remove(call);
        getPropertyChangeSupport().firePropertyChange("executingCalls", oldCalls, executingCalls);
    }

    /**
     * Gets an unmodifiable view of the currently executing tool calls.
     * 
     * @return The list of executing calls.
     */
    public List<AbstractToolCall<?, ?>> getExecutingCalls() {
        return Collections.unmodifiableList(executingCalls);
    }
}
