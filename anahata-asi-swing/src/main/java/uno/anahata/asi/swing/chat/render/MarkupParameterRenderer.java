/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.chat.render;

import javax.swing.JComponent;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A parameter renderer that falls back to standard markdown/HTML rendering.
 * 
 * @author anahata
 */
public class MarkupParameterRenderer implements ParameterRenderer<Object> {

    private MarkupTextSegmentRenderer renderer;

    /** No-arg constructor for factory instantiation. */
    public MarkupParameterRenderer() {}

    @Override
    public void init(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        String valStr = (value == null) ? "null" : value.toString();
        this.renderer = new MarkupTextSegmentRenderer(chatPanel, valStr, false);
    }

    @Override
    public JComponent getComponent() {
        return (renderer != null) ? renderer.getComponent() : null;
    }

    @Override
    public void updateContent(Object value) {
        if (renderer != null) {
            String valStr = (value == null) ? "null" : value.toString();
            renderer.updateContent(valStr);
        }
    }

    @Override
    public boolean render() {
        return (renderer != null) && renderer.render();
    }
}
