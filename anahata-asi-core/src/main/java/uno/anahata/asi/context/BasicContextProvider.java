/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;


/**
 * A basic, reusable implementation of the {@link ContextProvider} interface
 * that provides the foundational logic for hierarchical context management.
 * 
 * @author anahata
 */
@Getter
@RequiredArgsConstructor
public class BasicContextProvider implements ContextProvider {
    
    /** The unique identifier for this provider. */
    private final String id;
    
    /** The human-readable name of this provider. */
    private final String name;
    
    /** A detailed description of this provider's purpose. */
    private final String description;
    
    /** Whether this provider is currently active. Defaults to true. */
    @Setter
    private boolean providing = true;
    
    /** The parent provider in the hierarchy. */
    @Setter
    protected ContextProvider parent;

    /** The list of immediate child providers. */
    protected final List<ContextProvider> children = new ArrayList<>();

    /** {@inheritDoc} */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public ContextProvider getParentProvider() {
        return parent;
    }

    /** {@inheritDoc} */
    @Override
    public void setParentProvider(ContextProvider parent) {
        this.parent = parent;
    }
}
