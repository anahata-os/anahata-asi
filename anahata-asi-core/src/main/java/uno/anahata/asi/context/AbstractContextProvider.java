/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.util.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;


/**
 * The base class for all context providers, defining the contract for injecting
 * just-in-time context into an AI request. This is a direct port of the proven V1 design.
 * 
 * @author anahata
 */
@Getter
@RequiredArgsConstructor
public abstract class AbstractContextProvider implements ContextProvider{
    
    private final String id;
    
    private final String name;
    
    private final String description;
    
    @Setter
    private boolean enabled = true;
    
}
