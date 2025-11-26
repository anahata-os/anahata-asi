/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.context.system;

import java.util.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.ai.chat.Chat;


/**
 * The base class for all context providers, defining the contract for injecting
 * just-in-time context into an AI request. This is a direct port of the proven V1 design.
 * 
 * @author pablo
 */
@Getter
@RequiredArgsConstructor
public abstract class AbstractSystemInstructionsProvider {
    protected final Chat chat;

    private final String id;
    
    private final String name;
    
    private final String description;
    
    @Setter
    private boolean enabled = true;
    
    
    /**
     * Generates the context parts for this provider.
     * @param chat The current chat session.
     * @return A list of parts to be injected into the request.
     * @throws Exception if an error occurs during context generation.
     */
    public abstract List<String> getSystemInstructions() throws Exception;
    
}
