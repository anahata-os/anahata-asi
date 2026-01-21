/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.tool;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.tool.ToolContext;


/**
 * The base class for all AI toolkits. It integrates the tool execution context
 * with the hierarchical context provider system, allowing toolkits to natively
 * contribute system instructions and RAG data.
 * 
 * @author anahata
 */
//@Getter
@RequiredArgsConstructor
public abstract class AnahataToolkit extends ToolContext implements ContextProvider {
    
    /** Whether this toolkit is currently providing context augmentation. */
    @Setter
    @Getter
    private boolean providing = true;
    
    /**
     * The list of child context providers managed by this toolkit.
     */
    protected List<ContextProvider> childrenProviders = new ArrayList<>();
    
    /** {@inheritDoc} */
    @Override
    public String getId() {
        return toolkit.getName() + "@" + System.identityHashCode(this);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return toolkit.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return toolkit.getDescription();
    }

    /** {@inheritDoc} */
    @Override
    public ContextProvider getParentProvider() {
        return getToolManager();
    }

    @Override
    public List<ContextProvider> getChildrenProviders() {
        return childrenProviders;
    }
    
}
