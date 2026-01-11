/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JToggleButton;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.PropertyChangeSource;
import uno.anahata.ai.swing.icons.AutoPruneIcon;
import uno.anahata.ai.swing.icons.PinnedIcon;
import uno.anahata.ai.swing.icons.PrunedPartsIcon;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;

/**
 * A three-state toggle button for managing the pruning state of an
 * {@link AbstractPart} or {@link AbstractMessage}. It cycles through "Auto-Prune",
 * "Prune", and "Pin" states, updating the associated model object and its own
 * visual representation.
 *
 * @author anahata
 */
@Getter
public class PruningToggleButton extends JToggleButton {

    /** The AbstractPart or AbstractMessage this button is associated with. */
    @NonNull
    private final Object modelObject;

    /**
     * Constructs a new PruningToggleButton.
     *
     * @param modelObject The {@link AbstractPart} or {@link AbstractMessage} to control.
     */
    public PruningToggleButton(@NonNull Object modelObject) {
        if (!(modelObject instanceof AbstractPart || modelObject instanceof AbstractMessage)) {
            throw new IllegalArgumentException("Model object must be an AbstractPart or AbstractMessage.");
        }
        this.modelObject = modelObject;
        setMargin(new java.awt.Insets(0, 4, 0, 4));
        setAction(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                togglePruningState();
            }
        });
        updateVisualState(); // Initial visual state

        // Declarative, thread-safe binding to the "pruned" property
        new EdtPropertyChangeListener(this, (PropertyChangeSource) modelObject, "pruned", evt -> updateVisualState());
    }

    /**
     * Toggles the pruning state of the associated model object.
     * The cycle is: null (Auto-Prune) -> true (Prune) -> false (Pin) -> null.
     */
    private void togglePruningState() {
        Boolean currentPruned = getPrunedStateFromModel();
        Boolean newPruned;

        if (currentPruned == null) {
            newPruned = Boolean.TRUE; // Auto -> Prune
        } else if (currentPruned == Boolean.TRUE) {
            newPruned = Boolean.FALSE; // Prune -> Pin
        } else {
            newPruned = null; // Pin -> Auto
        }

        if (modelObject instanceof AbstractPart part) {
            part.setPruned(newPruned);
        } else if (modelObject instanceof AbstractMessage message) {
            message.setPruned(newPruned);
        }
    }

    /**
     * Retrieves the current pruned state from the associated model object.
     * @return The Boolean pruned state (true, false, or null).
     */
    private Boolean getPrunedStateFromModel() {
        if (modelObject instanceof AbstractPart part) {
            return part.getPruned();
        } else if (modelObject instanceof AbstractMessage message) {
            return message.isPruned();
        }
        return null;
    }

    /**
     * Updates the button's icon and tooltip based on the current pruning state.
     */
    private void updateVisualState() {
        Boolean prunedState = getPrunedStateFromModel();
        if (Boolean.TRUE.equals(prunedState)) {
            // Explicitly Pruned
            setIcon(new PrunedPartsIcon(16));
            setToolTipText("Status: Explicitly Pruned (Click to Pin)");
        } else if (Boolean.FALSE.equals(prunedState)) {
            // Explicitly Pinned
            setIcon(new PinnedIcon(16));
            setToolTipText("Status: Explicitly Pinned (Click for Auto-Prune)");
        } else {
            // Auto-Prune (Default)
            setIcon(new AutoPruneIcon(16));
            setToolTipText("Status: Auto-Prune (Click to Prune)");
        }
    }
}
