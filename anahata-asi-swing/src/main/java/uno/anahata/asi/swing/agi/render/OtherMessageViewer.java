/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import javax.swing.JScrollPane;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A specialized scroll pane for rendering any {@link AbstractMessage}. 
 * This is useful for displaying messages in secondary UI locations 
 * like toolkit details or system instruction previews.
 *
 * @author anahata
 */
public class OtherMessageViewer extends JScrollPane {

    private final AgiPanel agiPanel;
    private final AbstractMessage message;
    private final AbstractMessagePanel<AbstractMessage> messagePanel;

    /**
     * Constructs a new OtherMessageViewer.
     *
     * @param agiPanel The parent agi panel.
     * @param message The message to render.
     * @param renderPruneButtons Whether to render pruning controls.
     * @param renderRemoveButtons Whether to render remove controls.
     */
    public OtherMessageViewer(@NonNull AgiPanel agiPanel, @NonNull AbstractMessage message, 
                             boolean renderPruneButtons, boolean renderRemoveButtons) {
        this.agiPanel = agiPanel;
        this.message = message;

        this.messagePanel = new OtherMessagePanel(agiPanel, message, renderPruneButtons, renderRemoveButtons);

        setViewportView(messagePanel);
        setBorder(null);
        setOpaque(false);
        getViewport().setOpaque(false);
        setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        getVerticalScrollBar().setUnitIncrement(16);
    }
    
    /**
     * Triggers a re-render of the underlying message panel.
     */
    public void render() {
        messagePanel.render();
    }
}
