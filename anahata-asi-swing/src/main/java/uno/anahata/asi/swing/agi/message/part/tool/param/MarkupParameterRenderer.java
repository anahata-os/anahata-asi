/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRenderer;
import javax.swing.JComponent;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.text.MarkupTextSegmentRenderer;

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
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        String valStr = (value == null) ? "null" : value.toString();
        this.renderer = new MarkupTextSegmentRenderer(agiPanel, valStr, false);
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
