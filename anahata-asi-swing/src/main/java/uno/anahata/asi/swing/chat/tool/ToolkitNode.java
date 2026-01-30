/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A context tree node representing an {@link AbstractToolkit}.
 * <p>
 * This node acts as a bridge in the hierarchy. It exposes the toolkit's 
 * internal {@link ContextProvider} (the actual tool instance) as a child 
 * named "Context", followed by all the individual tools defined within the toolkit.
 * </p>
 *
 * @author anahata
 */
public class ToolkitNode extends AbstractContextNode<AbstractToolkit<?>> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new ToolkitNode.
     * @param userObject The toolkit to wrap.
     */
    public ToolkitNode(AbstractToolkit<?> userObject) {
        super(userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return userObject.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /**
     * {@inheritDoc}
     * Implementation details:
     * 1. If the toolkit has a context provider, it adds it as a child 
     *    ProviderNode with the display name "Context".
     * 2. It then adds all tools in the toolkit as child ToolNodes.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            
            // 1. The toolkit's context provider implementation (if any)
            ContextProvider cp = userObject.getContextProvider();
            if (cp != null) {
                children.add(new ProviderNode(cp) {
                    @Override
                    public String getName() {
                        return "Context";
                    }
                });
            }
            
            // 2. The tools
            for (AbstractTool<?, ?> tool : userObject.getAllTools()) {
                children.add(new ToolNode(tool));
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = userObject.getTokenCount();
        this.historyTokens = 0;
        this.ragTokens = 0;
        this.status = userObject.isEnabled() ? "Enabled" : "Disabled";
        
        for (AbstractContextNode<?> child : getChildren()) {
            child.refreshTokens();
        }
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return IconUtils.getIcon("java.png", 16, 16);
    }
}
