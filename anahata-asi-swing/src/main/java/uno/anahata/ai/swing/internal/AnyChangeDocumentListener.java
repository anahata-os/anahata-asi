package uno.anahata.ai.swing.internal;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * A utility {@link DocumentListener} that fires a single callback for any type of
 * document change (insert or remove). This simplifies the implementation for UI
 * components that just need to know that "something changed" without handling
 * each event type separately.
 *
 * @author Anahata
 */
public class AnyChangeDocumentListener implements DocumentListener {

    private final Runnable onChange;

    /**
     * Creates a new listener.
     *
     * @param onChange The {@link Runnable} to execute when the document changes.
     */
    public AnyChangeDocumentListener(Runnable onChange) {
        this.onChange = onChange;
    }

    @Override
    public void insertUpdate(DocumentEvent e) {
        onChange.run();
    }

    @Override
    public void removeUpdate(DocumentEvent e) {
        onChange.run();
    }

    @Override
    public void changedUpdate(DocumentEvent e) {
        // Plain text components do not fire this event, but we call it for completeness.
        onChange.run();
    }
}
