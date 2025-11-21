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
package uno.anahata.ai.model.provider;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;

/**
 * Manages available {@link AiProvider} implementations via programmatic registration.
 * <p>
 * This class allows the application to explicitly control which AI providers are active.
 * It is the central point for the core framework to find and interact with all registered
 * AI provider modules.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class AiProviderRegistry {

    private final Set<AbstractAiProvider> registeredProviders = new LinkedHashSet<>();

    /**
     * Programmatically registers a new AI provider instance.
     * If a provider of the same class is already registered, this method will replace it.
     *
     * @param provider The provider instance to register.
     */
    public synchronized void registerProvider(AbstractAiProvider provider) {
        log.info("Programmatically registering AI provider: {}", provider.getProviderId());
        // Remove if an instance of the same class already exists to allow replacement
        registeredProviders.removeIf(p -> p.getClass().equals(provider.getClass()));
        registeredProviders.add(provider);
    }

    /**
     * Gets a list of all programmatically registered AI providers.
     *
     * @return A list of all unique {@link AiProvider} instances.
     */
    public synchronized List<AbstractAiProvider> getProviders() {
        if (registeredProviders.isEmpty()) {
            log.warn("No Anahata AI providers have been registered. The application will not have access to any AI models.");
        }
        return new ArrayList<>(registeredProviders);
    }
}
