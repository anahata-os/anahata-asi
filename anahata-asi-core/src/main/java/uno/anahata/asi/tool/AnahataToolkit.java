/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.tool;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.tool.ToolContext;


/**
 * The base class for all AI toolkits. It integrates the tool execution context
 * with the hierarchical context provider system, allowing toolkits to natively
 * contribute system instructions and RAG data.
 * 
 * @author anahata
 */
@Slf4j
@RequiredArgsConstructor
public abstract class AnahataToolkit extends ToolContext implements ContextProvider, Rebindable {
    
    /** Whether this toolkit is currently providing context augmentation. */
    @Setter
    @Getter
    private boolean providing = true;
    
    /**
     * The list of child context providers managed by this toolkit.
     * Uses CopyOnWriteArrayList for thread-safe concurrent access during IDE events.
     */
    protected List<ContextProvider> childrenProviders = new CopyOnWriteArrayList<>();
    
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

    @Override
    public void rebind() {
        log.info("Rebinding toolkit: {}", getName());
        // Toolkits can override this to restore transient state
    }
    
}
