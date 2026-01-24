/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.AbstractPart;

/**
 * A context tree node representing a single part of a message.
 *
 * @author anahata
 */
public class PartNode extends AbstractContextNode<AbstractPart> {

    public PartNode(AbstractPart userObject) {
        super(userObject);
    }

    @Override
    public String getName() {
        return userObject.getClass().getSimpleName() + " #" + userObject.getSequentialId();
    }

    @Override
    public String getDescription() {
        return "A " + userObject.getClass().getSimpleName() + " part.";
    }

    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
    }

    @Override
    public int getInstructionsTokens(Chat chat) {
        return 0;
    }

    @Override
    public int getDeclarationsTokens() {
        return 0;
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        return userObject.getTokenCount();
    }

    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    @Override
    public String getStatus() {
        if (userObject.isEffectivelyPruned()) {
            return "Pruned (" + userObject.getTurnsLeft() + " turns left)";
        }
        return "Active";
    }
}
