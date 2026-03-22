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

    /** The underlying segment renderer used for the actual markup transformation. */
    private MarkupTextSegmentRenderer renderer;

    /** No-arg constructor for factory instantiation. */
    public MarkupParameterRenderer() {}

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Wraps the value in a {@link MarkupTextSegmentRenderer} 
     * to support standard Markdown/HTML formatting within the tool call.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        String valStr = (value == null) ? "null" : value.toString();
        this.renderer = new MarkupTextSegmentRenderer(agiPanel, valStr, false);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Delegates to the underlying markup renderer's component.</p>
     */
    @Override
    public JComponent getComponent() {
        return (renderer != null) ? renderer.getComponent() : null;
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Resolves the object to a string and pushes it 
     * into the markup pipeline.</p>
     */
    @Override
    public void updateContent(Object value) {
        if (renderer != null) {
            String valStr = (value == null) ? "null" : value.toString();
            renderer.updateContent(valStr);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Triggers the visual refresh of the markup component.</p>
     */
    @Override
    public boolean render() {
        return (renderer != null) && renderer.render();
    }
}
