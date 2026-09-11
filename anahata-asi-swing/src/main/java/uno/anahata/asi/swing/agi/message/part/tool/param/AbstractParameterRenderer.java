/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Abstract base class for parameter renderers implementing standard context
 * binding and hierarchical composite notifications via {@link #valueChanged(Object)}.
 *
 * @param <T> The type of parameter value this renderer handles.
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractParameterRenderer<T> implements ParameterRenderer<T> {

    /** The parent agi panel providing session context. */
    protected AgiPanel agiPanel;

    /** The tool call being rendered. */
    protected AbstractToolCall<?, ?> call;

    /** The name of the parameter. */
    protected String paramName;

    /** The current value. */
    protected T value;

    /** The enclosing composite parent renderer, or null if top-level. */
    protected ParameterRenderer<?> parentRenderer;

    /**
     * {@inheritDoc}
     * <p>Stores the session context, tool call, parameter name, and initial value.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, T value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.value = value;
    }

    /**
     * Authoritatively propagates an updated value:
     * <ul>
     * <li>If nested inside a parent renderer, notifies the parent via {@link #childValueChanged}.</li>
     * <li>If top-level (parentRenderer == null), commits directly to {@code call.setModifiedArgument(paramName, newValue)}.</li>
     * </ul>
     *
     * @param newValue The newly updated value.
     */
    public void valueChanged(T newValue) {
        this.value = newValue;
        if (parentRenderer != null) {
            parentRenderer.childValueChanged(this, newValue);
        } else if (call != null && paramName != null) {
            call.setModifiedArgument(paramName, newValue);
        }
    }
}
