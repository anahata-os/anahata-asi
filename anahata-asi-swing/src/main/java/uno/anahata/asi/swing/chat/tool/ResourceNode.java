/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * A context tree node representing a single managed resource.
 * <p>
 * This node displays the resource's name, its position in the prompt 
 * (System Instructions or Prompt Augmentation), and its refresh policy.
 * </p>
 *
 * @author anahata
 */
public class ResourceNode extends AbstractContextNode<AbstractResource<?, ?>> {

    /**
     * Constructs a new ResourceNode.
     * @param userObject The resource to wrap.
     */
    public ResourceNode(AbstractResource<?, ?> userObject) {
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
     * Implementation details: A resource is always a leaf node.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        int tokens = userObject.getTokenCount();
        
        this.instructionsTokens = userObject.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS ? tokens : 0;
        this.declarationsTokens = 0;
        this.historyTokens = 0;
        this.ragTokens = userObject.getContextPosition() == ContextPosition.PROMPT_AUGMENTATION ? tokens : 0;
        
        this.status = userObject.getContextPosition().name() + " | " + userObject.getRefreshPolicy().name();
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return IconUtils.getIcon(userObject.getIconId());
    }
}
