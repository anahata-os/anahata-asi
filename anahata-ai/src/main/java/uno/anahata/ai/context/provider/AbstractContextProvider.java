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
package uno.anahata.ai.context.provider;

import java.util.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.ai.Chat;
import uno.anahata.ai.model.core.AbstractPart;

/**
 * The base class for all context providers, defining the contract for injecting
 * just-in-time context into an AI request. This is a direct port of the proven V1 design.
 * 
 * @author pablo
 */
@Getter
@RequiredArgsConstructor
public abstract class AbstractContextProvider {
    
    private final String id;
    
    private final String name;
    
    private final String description;
    
    private final ContextPosition position;
    
    @Setter
    private boolean enabled = true;
    
    /**
     * Generates the context parts for this provider.
     * @param chat The current chat session.
     * @return A list of parts to be injected into the request.
     * @throws Exception if an error occurs during context generation.
     */
    public abstract List<AbstractPart> getParts(Chat chat) throws Exception;
    
}
