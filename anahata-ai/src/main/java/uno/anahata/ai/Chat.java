/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.config.ChatConfig;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.ModelMessage;
import uno.anahata.ai.model.core.Request;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.tool.ToolManager;

/**
 * The central, provider-agnostic orchestrator for a single chat session in the V2 architecture.
 * This class manages the conversation history, orchestrates calls to the AI provider,
 * and handles the tool execution lifecycle.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
public class Chat {

    private final ChatConfig config;
    private final ToolManager toolManager;
    private final ExecutorService executor;
    private final List<AbstractAiProvider> providers = new ArrayList<>();

    // The complete, canonical conversation history for this session.
    private final List<AbstractMessage> history = new ArrayList<>();

    /** The currently selected model for the chat session. */
    @Setter
    private AbstractModel selectedModel;

    /** The configuration for the next generateContent request. */
    @Setter
    private RequestConfig requestConfig;

    @SneakyThrows
    public Chat(@NonNull ChatConfig config) {
        this.config = config;
        this.executor = AnahataExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.toolManager = new ToolManager(this);
        this.requestConfig = RequestConfig.builder().build(); // Initialize with a default
        
        // Discover and instantiate providers
        for (Class<? extends AbstractAiProvider> providerClass : config.getProviderClasses()) {
            try {
                // Instantiate the provider, passing this Chat instance.
                AbstractAiProvider provider = providerClass.getConstructor(Chat.class).newInstance(this);
                this.providers.add(provider);
                log.info("Successfully instantiated and registered provider: {}", provider.getProviderId());
            } catch (Exception e) {
                log.error("Failed to instantiate provider class: {}", providerClass.getName(), e);
            }
        }
    }

    /**
     * Sends a user message to the currently selected AI model and returns the response.
     * This method does NOT modify the conversation history. The caller is responsible
     * for choosing a candidate from the response and adding it to the history via
     * the {@link #chooseCandidate(ModelMessage)} method.
     *
     * @param message The user's message.
     * @return The model's response.
     */
    public Response sendMessage(UserMessage message) {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message. Call setSelectedModel().");
        }
        
        history.add(message);
        
        Request request = new Request(
            selectedModel,
            Collections.unmodifiableList(history),
            requestConfig
        );

        log.info("Sending request to model '{}' with {} messages in history.", selectedModel.getModelId(), history.size());
        Response response = selectedModel.generateContent(request);
        
        // TODO: Implement the full tool processing logic from V1's Chat.java
        
        return response;
    }
    
    /**
     * Adds a chosen model message (a candidate from a Response) to the conversation history.
     * @param message The model message to add.
     */
    public void chooseCandidate(@NonNull ModelMessage message) {
        history.add(message);
    }
    
    /**
     * Gets a flattened list of all models available from all registered providers.
     * @return A list of all available models.
     */
    public List<AbstractModel> getAllModels() {
        return providers.stream()
                .flatMap(provider -> provider.getModels().stream())
                .collect(Collectors.toList());
    }
    
    public void shutdown() {
        log.info("Shutting down Chat for session {}", config.getSessionId());
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }
}
