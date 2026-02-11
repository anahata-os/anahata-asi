/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.tool.ToolManager;

/**
 * A context tree node representing a {@link ContextProvider}.
 * <p>
 * This node handles the recursive structure of context providers. It has 
 * special logic for the {@link ToolManager}, which exposes its registered 
 * toolkits as child nodes.
 * </p>
 *
 * @author anahata
 */
public class ProviderNode extends AbstractContextNode<ContextProvider> {

    /**
     * Constructs a new ProviderNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The context provider to wrap.
     */
    public ProviderNode(ChatPanel chatPanel, ContextProvider userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        String name = userObject.getName();
        if (name != null && name.contains("<") && !name.toLowerCase().startsWith("<html>")) {
            return "<html>" + name + "</html>";
        }
        return name;
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        List<Object> objects = new ArrayList<>();
        if (userObject instanceof ToolManager tm) {
            objects.addAll(new ArrayList<>(tm.getToolkits().values()));
        } else {
            for (ContextProvider child : userObject.getChildrenProviders()) {
                if (!(child instanceof AbstractToolkit)) {
                    objects.add(child);
                }
            }
        }
        return objects;
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractToolkit<?> tk) {
            return new ToolkitNode(chatPanel, tk);
        } else if (obj instanceof ContextProvider cp) {
            return new ProviderNode(chatPanel, cp);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        this.instructionsTokens = userObject.getInstructionsTokenCount();
        this.ragTokens = userObject.getRagTokenCount();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else if (!userObject.isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = "Providing";
        }
    }
}
